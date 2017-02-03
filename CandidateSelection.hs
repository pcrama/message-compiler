module CandidateSelection

where

import Data.Ord (comparing)
import Data.List (sort
                , sortBy
                , break)
import Data.Map (toList)
import Data.Array (assocs)
import qualified Data.ByteString as B
import Data.ByteString.Builder
import qualified Data.ByteString.Search as BS
import qualified Data.ByteString.Lazy as BL

import Utils
import InputText
import Digram
import EnnGram

data ReplacementInfo = ReplacementInfo { lenToReplace :: Int -- how many bytes to remove from original
                                       , codepoint :: Codepoint -- what to put in place
                                       , firstPos :: Int -- there's always at least one, otherwise, it wouldn't be a candidate
                                       , positions :: [Int] -- positions where this replacement can be made
                                       }
  deriving (Show, Eq)

applyCompression :: InputText -> [(Candidate, Codepoint)] -> InputText
applyCompression input cands =
      BL.toStrict
    $ toLazyByteString
    $ (`mappend` foldMap (\(Candidate c _, _) -> word8 0 `mappend` byteString c) cands)
    $ go 0 mempty input $ map (\(Candidate c _, cp) ->
                                  let pos = BS.nonOverlappingIndices c input
                                  in ReplacementInfo { lenToReplace = B.length c
                                                     , codepoint = cp
                                                     -- pos is never null because there are always at least
                                                     -- two matches, otherwise, it wouldn't be a candidate
                                                     , firstPos = head pos
                                                     , positions = tail pos})
                              cands
  where go :: Int -> Builder -> B.ByteString -> [ReplacementInfo] -> Builder
        go offs b input [] = b `mappend` byteString (B.drop offs input)
        go offs b input cands =
            let (nextCandLen, nextCp, otherCands, candOffs) = selectNextCand cands
            in go (candOffs + nextCandLen)
                  (mappend b
                         $ mappend (byteString . B.take (candOffs - offs) . B.drop offs $ input)
                                   (word8 nextCp))
                  input
                  otherCands

selectNextCand :: [ReplacementInfo] -> (Int, Codepoint, [ReplacementInfo], Int)
selectNextCand [] = error "Should never happen: selectNextCand called with empty list"
selectNextCand (replInfo:xs) = improve replInfo xs []
  where improve replInfo [] xs = (lenToReplace replInfo
                                 , codepoint replInfo
                                 , if null (positions replInfo)
                                   then xs
                                   else replInfo { firstPos = head $ positions replInfo
                                                 , positions = tail $ positions replInfo
                                        }:xs
                                 , firstPos replInfo)
        improve curr (next:lcos) xs =
            let preferCurr = improve curr lcos (next:xs)
                preferNext = improve next lcos (curr:xs)
            in case comparing firstPos curr next of
                 LT -> preferCurr -- apply first occurring replacement
                 EQ -> case comparing lenToReplace curr next of -- TODO: is this even necessary?
                         LT -> preferNext -- prefer longer replacements
                         EQ -> error "Should never happen: curr and next can't be the same candidate"
                         GT -> preferCurr
                 GT -> preferNext

data Candidate = Candidate B.ByteString Count
  deriving (Show, Eq)

instance Ord Candidate where
  compare (Candidate s1 c1) (Candidate s2 c2) =
    let l1 = B.length s1
        l2 = B.length s2 in
    case (comparing $ uncurry compressionGain)
           (l1, c1) (l2, c2) of
      GT -> GT
      LT -> LT
      EQ -> case compare c1 c2 of
              -- swapped comparison results to prefer
              -- candidates that appear less often (lower
              -- probability to invalidate other candidates
              -- found during same run)
              GT -> LT
              LT -> GT
              -- gain and count equal implies that lengths
              -- are equal. Ordering the compression
              -- candidates lexicographically is handy for
              -- the tests and necessary to maintain
              -- consistency with Eq instance.
              EQ -> compare s1 s2

makeCandidates :: EnnGramMap -> DigramTable
               -> [Candidate]
makeCandidates mp dt = mergeBy (flip compare) ennGrams diGrams
  where diGrams = map digramToCand
                $ take maxCompressions
                $ sortBy (flip $ comparing digramCount)
                $ filter ((>= minCountLen2) . digramCount)
                $ assocs dt
        digramCount (_, count) = count
        digramToCand (di, count) =
            Candidate (unDigram di) count
        worstDigramGain =
            if diGrams `longerThan` (maxCompressions - 1)
            then let Candidate s c = head $ reverse diGrams
                 -- B.length s should always be 2 (that's what a Digram is...)
                 in compressionGain (B.length s) c
            else 0
        -- can't `take maxCompressions' here because if the
        -- text contains AVeryLongString more than once, the
        -- candidates will contain all substrings of this
        -- string, but replacing it would void all those
        -- others, meaning there were more candidates to
        -- look at.
        ennGrams = sortBy (flip compare)
                 $ map fullEGToCand
                 $ getEnnGrams worstDigramGain mp
        fullEGToCand :: (FullEnnGram, CombineState2) -> Candidate
        fullEGToCand (FullEnnGram eg, CS2 (_, count)) = Candidate eg count

getNextCandidates :: [Candidate] -> [Candidate]
getNextCandidates [] = []
getNextCandidates (x:xs) = x:(getNextCandidates rest)
  where (rest, _) = break (overlaps x) $
                          filter (not . dropSafeOverlap x) xs

-- These overlaps are safe to drop: when "abcdef" is a
-- candidate, "abcde" or "bcdef" etc will be candidates,
-- too.  However, `forgetting' these shorter candidates
-- when selecting the longest is OK if the shorter all
-- occurred only in the longest (i.e . have the same
-- occurrence count).  It is tempting to try and simply
-- adjust the count of shorter candidates occurring more
-- often (less often would point to a bug), but this would
-- require to sort the resulting list again and I'm afraid
-- of the extra complexity.
dropSafeOverlap :: Candidate -> Candidate -> Bool
dropSafeOverlap (Candidate chosen cch)
                (Candidate other cot) =
     cch == cot
  && B.length chosen > B.length other
  && other `B.isInfixOf` chosen

overlaps :: Candidate -> Candidate -> Bool
overlaps (Candidate chosen _) (Candidate other _) =
     or (map (`B.isPrefixOf` other)
         $ filter (not . B.null)
         $ B.tails chosen)
  || or (map (`B.isPrefixOf` chosen)
         $ filter (not . B.null)
         $ B.tails other)
  || let (shorter, longer) = if B.length chosen > B.length other
                             then (other, chosen)
                             else (chosen, other)
     in shorter `B.isInfixOf` longer

getEnnGrams :: Int -> EnnGramMap -> [(FullEnnGram, CombineState2)]
getEnnGrams floor = filter ((> floor) . compressionGain')
                   . toList
  where ascendingEnnGain x y = (compare `on` compressionGain') y x
        compressionGain' (FullEnnGram ng, CS2 (_, count)) =
          compressionGain (B.length ng) count

mergeBy _ xs [] = xs
mergeBy _ [] ys = ys
mergeBy f xx@(x:xs) yy@(y:ys) =
  case f x y of
    GT -> y:mergeBy f xx ys
    otherwise -> x:mergeBy f xs yy

merge :: Ord a => [a] -> [a] -> [a]
merge = mergeBy compare

module CandidateSelection

where

import Data.Ord (comparing)
import Data.List (sort
                , sortBy
                , break)
import Data.Map (toList)
import Data.Array (assocs)
import qualified Data.ByteString as B

import Utils
import InputText
import Digram
import EnnGram

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

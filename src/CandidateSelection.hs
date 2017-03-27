module CandidateSelection

where

import Data.Ord (comparing)
import Data.List (sortBy
                , break)
import Data.Map (toList)
import Data.Array (assocs)
import qualified Data.ByteString as B

import Utils
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

makeCandidates :: Int -> EnnGramMap -> DigramTable
               -> [Candidate]
makeCandidates maxN mp dt = mergeBy (flip compare) ennGrams diGrams
  where diGrams = map digramToCand
                $ take maxN
                $ getNextDigrams
                $ sortBy (flip $ comparing digramCount)
                $ filter ((>= minCountLen2) . digramCount)
                $ assocs dt
        digramCount (_, count) = count
        digramToCand (di, count) =
            Candidate (unDigram di) count
        worstDigramGain =
            if (not . null) diGrams && diGrams `longerThan` (maxN - 1)
            then let Candidate _ c = head $ reverse diGrams
                 -- B.length s should always be 2 (that's what a Digram is...)
                 in compressionGain 2 c
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

-- Identical to getNextCandidates but specialised for Digrams only.
getNextDigrams :: [(Digram, Count)] -> [(Digram, Count)]
getNextDigrams [] = []
getNextDigrams (x:xs) = x:(getNextDigrams $ safeToKeep ++ sameGain)
  where (safeToKeep, rest) = break (overlapsDigram `on` fst $ x) xs
                              -- there is no safe overlap for any 2 Digram combination
                              -- so the filter operation is not needed.
                              -- $ filter (not . dropSafeOverlap x) xs
        -- No need to compute the gain, all candidates in this list
        -- have the same length anyway, so we can just compare the
        -- lengths
        candComprGain (_, c) = c
        -- `sameGain' are all candidates with the same gain as the
        -- first element of `rest'.  We can't keep the first element
        -- because it overlaps with `x' and as such after `x` is
        -- replaced, its count may drop.  However, all other
        -- candidates with the same gain that do not overlap can
        -- already be used.
        sameGain = case rest of
                     [] -> []
                     (y:ys) -> let gain = candComprGain y
                               in foldr (keepOthersWithSameGain gain)
                                        []
                                        $ filter (not . (overlapsDigram `on` fst $ x)) ys
        keepOthersWithSameGain g z zs =
          if candComprGain z >= g
          then z:zs
          -- input list is sorted -> no need to look further once the gain decreases
          else []
  
getNextCandidates :: [Candidate] -> [Candidate]
getNextCandidates [] = []
getNextCandidates (x:xs) = x:(getNextCandidates $ safeToKeep ++ sameGain)
  where (safeToKeep, rest) = break (overlaps x)
                                 $ filter (not . dropSafeOverlap x) xs
        candComprGain (Candidate s c) = compressionGain (B.length s) c
        -- `sameGain' are all candidates with the same gain as the
        -- first element of `rest'.  We can't keep the first element
        -- because it overlaps with `x' and as such after `x` is
        -- replaced, its count may drop.  However, all other
        -- candidates with the same gain that do not overlap can
        -- already be used.
        sameGain = case rest of
                     [] -> []
                     (y:ys) -> let gain = candComprGain y
                               in foldr (keepOthersWithSameGain gain)
                                        []
                                        $ filter (not . overlaps x) ys
        keepOthersWithSameGain g z zs =
          if candComprGain z >= g
          then z:zs
          -- input list is sorted -> no need to look further once the gain decreases
          else []

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
getEnnGrams lowest = filter ((> lowest) . compressionGain')
                   . toList
  where compressionGain' (FullEnnGram ng, CS2 (_, count)) =
          compressionGain (B.length ng) count

mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy _ xs [] = xs
mergeBy _ [] ys = ys
mergeBy f xx@(x:xs) yy@(y:ys) =
  case f x y of
    GT -> y:mergeBy f xx ys
    _ -> x:mergeBy f xs yy

merge :: Ord a => [a] -> [a] -> [a]
merge = mergeBy compare

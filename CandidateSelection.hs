module CandidateSelection

where

import Data.Ord (comparing)
import Data.List (sort
                , sortBy
                , tails
                , break
                , isPrefixOf
                , isSuffixOf
                , isInfixOf)
import Data.Map (toList)
import Data.Array (assocs)

import Utils
import InputText
import Digram
import EnnGram

data Candidate = Candidate String Length Count
  deriving (Show, Eq)

instance Ord Candidate where
  compare (Candidate s1 l1 c1) (Candidate s2 l2 c2) =
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
            Candidate (unDigram di) 2 count
        worstDigramGain =
            if diGrams `longerThan` (maxCompressions - 1)
            then let Candidate _ l c = head $ reverse diGrams
                 in compressionGain l c
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
        fullEGToCand (fe@(FullEnnGram eg _), CS2 (_, count)) =
          Candidate (fullEnnGramToString fe)
                    (ennLength eg)
                    count

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
dropSafeOverlap (Candidate chosen lch cch)
                (Candidate other lot cot) =
     cch == cot
  && lch > lot
  && other `isInfixOf` chosen

overlaps :: Candidate -> Candidate -> Bool
overlaps (Candidate chosen lc _) (Candidate other lo _) =
     or (map (`isPrefixOf` other)
         $ filter (not . null)
         $ tails chosen)
  || or (map (`isPrefixOf` chosen)
         $ filter (not . null)
         $ tails other)
  || let (shorter, longer) = if lc > lo
                             then (other, chosen)
                             else (chosen, other)
     in shorter `isInfixOf` longer

getEnnGrams :: Int -> EnnGramMap -> [(FullEnnGram, CombineState2)]
getEnnGrams floor = filter ((> floor) . compressionGain')
                   . toList
  where ascendingEnnGain x y = (compare `on` compressionGain') y x
        compressionGain' (FullEnnGram ng _, CS2 (_, count)) =
          compressionGain (ennLength ng) count

mergeBy _ xs [] = xs
mergeBy _ [] ys = ys
mergeBy f xx@(x:xs) yy@(y:ys) =
  case f x y of
    GT -> y:mergeBy f xx ys
    otherwise -> x:mergeBy f xs yy

merge :: Ord a => [a] -> [a] -> [a]
merge = mergeBy compare

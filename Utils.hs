module Utils

where

import Data.List (foldl')
import Data.Ix (Ix, range)
import Data.Array ((!), Array, accumArray, array, bounds)

type Offset = Int
type Length = Int

-- Assuming that `compressionGain' is well-behaved, these
-- constants allow to avoid calling compressionGain during
-- predicates where the length of the Digram or EnnGram is
-- already known.
minCountLen2 = head $ dropWhile ((<=0) . (compressionGain 2)) [2..]
minCountLen3 = head $ dropWhile ((<=0) . (compressionGain 3)) [2..]
minCountLen4 = head $ dropWhile ((<=0) . (compressionGain 4)) [2..]

compressionGain :: Length -> Int -> Int
compressionGain len count =
  (count - 1) * (fromIntegral len - 1) - 2

foldEnumArray :: Ix a => ((a, b) -> c -> c) -> c -> Array a b -> c
foldEnumArray fun base arr =
    foldl' combine base (range $ bounds arr)
  where combine xs idx = fun (idx, (arr ! idx)) xs

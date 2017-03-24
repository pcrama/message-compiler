module Utils

where

import Data.List (foldl')
import Data.Ix (Ix, range)
import Data.Array ((!), Array, bounds)
import Data.Word (Word8)
import qualified Data.ByteString as B

type Offset = Int
type Length = Int
type Count = Int

-- All strings in the input text are pasted together into one big
-- string, separated by this code point
stringSeparationCP :: Integral a => a
stringSeparationCP = 0

-- Assuming that `compressionGain' is well-behaved, these
-- constants allow to avoid calling compressionGain during
-- predicates where the length of the Digram or EnnGram is
-- already known.
minCountLen2, minCountLen3, minCountLen4 :: Length
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

foldEnumByteString :: ((Int, Word8) -> c -> c) -> c -> B.ByteString -> c
foldEnumByteString fun base =
    snd . B.foldl' (injectCounter fun) (0, base)
  where injectCounter f (cnt, acc) cp = (cnt + 1, f (cnt, cp) acc)

-- found `on' with Hoogle but not in Hugs?
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on f g x y = f (g x) (g y)

-- how many `special chars' are available to represent substrings
maxCompressions :: Int
maxCompressions = 64

-- last codepoint that represents itself, not a substring.
-- Note: lastPlainCodepoint + maxCompressions <= (maxBound :: CodePoint)
lastPlainCodepoint :: Int
lastPlainCodepoint = 191

firstCompressionMarker :: Int
firstCompressionMarker = lastPlainCodepoint + 1

longerThan :: (Num t, Ord t) => [a] -> t -> Bool
longerThan [] x = x < 0
longerThan (_:xs) len = longerThan xs (len - 1)

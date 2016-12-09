module Digram (
  Digram(..)
, DigramTable
, digramTable
, unDigram
, summarizeDigramTable
, initDigram
, shiftDigram
, stringBoundary
)

where

import Data.Array ((!), Array, accumArray, array, bounds)
import Data.Bits ((.&.), (.|.), shift)
import Data.List (foldl')
import Data.Ix (Ix, range)
import Data.Char (chr, ord)
import Data.Word (Word16)

import InputText (Codepoint, InputText)
import Utils (Offset)

newtype Digram = Digram Word16
  deriving (Bounded, Eq, Ix, Ord)

type DigramTable = Array Digram Offset

initDigram :: Digram
initDigram = Digram 0

summarizeDigramTable :: DigramTable -> [(String, Offset)]
summarizeDigramTable =
  foldEnumArray (\(di,count) t ->
                   if count > 0
                   then (unDigram di, count):t
                   else t)
                []

instance Show Digram where
  show x = "Digram " ++ unDigram x

unDigram (Digram x) = 
    (wchr (x `shift` (-8))):(wchr $ x .&. 0xff):[]
  where wchr = chr . fromIntegral

shiftDigram :: Codepoint -> Digram -> Digram
shiftDigram c (Digram x) = Digram $
  ((x .&. 0xff) `shift` 8) .|. ((fromInteger . toInteger) c .&. 0xff)

stringBoundary :: Digram -> Bool
stringBoundary (Digram x) = (x < 256) || ((x .&. 255) == 0)

-- Return array from Digram to occurence count.  This info
-- will be used to abort generation of EnnGram candidates to
-- count on the basis that if an EnnGram contains a Digram
-- that only occurs once, that EnnGram will also only occur
-- once and hence it makes no sense to count it in the hope
-- of compressing it.  This mechanism is also used to
-- prevent compression of parts across string boundaries
-- (the input is a list of strings represented as InputText)
-- by forcing all Digram containing the string separator
-- (\NUL) to an occur count of 0.
digramTable :: InputText -> DigramTable
digramTable txt =
    accumArray (+) 0 (minBound, maxBound) . map (flip (,) 1) $
      digramList
  where digramList = filter (not . stringBoundary) . snd $
                            foldArray tally (False, [Digram 0]) txt
        tally :: Codepoint -> (Bool, [Digram]) -> (Bool, [Digram])
        tally c (riskOverlap, z@(x:_)) =
          let newDigram = shiftDigram c x in
           if riskOverlap && (newDigram == x)
           then (False, z)
           else (True, newDigram:z)

foldArray :: Ix a => (b -> c -> c) -> c -> Array a b -> c
foldArray fun base arr =
    foldl' combine base (range $ bounds arr)
  where combine xs idx = fun (arr ! idx) xs

foldEnumArray :: Ix a => ((a, b) -> c -> c) -> c -> Array a b -> c
foldEnumArray fun base arr =
    foldl' combine base (range $ bounds arr)
  where combine xs idx = fun (idx, (arr ! idx)) xs

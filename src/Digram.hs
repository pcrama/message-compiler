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

import Data.Array ((!), Array, accumArray, bounds)
import Data.Bits ((.&.), (.|.), shift)
import Data.List (foldl')
import Data.Ix (Ix, range)
import Data.Char (chr)
import Data.Word (Word16)
import qualified Data.ByteString as B

import InputText (Codepoint, InputText)
import Utils (Count, stringSeparationCP)

newtype Digram = Digram Word16
  deriving (Bounded, Eq, Ix, Ord)

type DigramTable = Array Digram Count

initDigram :: Digram
initDigram = Digram 0

summarizeDigramTable :: DigramTable -> [(B.ByteString, Count)]
summarizeDigramTable =
  foldEnumArray (\(di,count) t ->
                   if count > 0
                   then (unDigram di, count):t
                   else t)
                []

instance Show Digram where
  show x = "Digram " ++ (map (chr .fromIntegral) . B.unpack $ unDigram x)

unDigram :: Digram -> B.ByteString
unDigram (Digram x) = 
    B.pack $ (wchr (x `shift` (-8))):(wchr $ x .&. 0xff):[]
  where wchr = fromIntegral

shiftDigram :: Codepoint -> Digram -> Digram
shiftDigram c (Digram x) = Digram $
  ((x .&. 0xff) `shift` 8) .|. ((fromInteger . toInteger) c .&. 0xff)

stringBoundary :: Digram -> Bool
stringBoundary (Digram x) = ((x `shift` (-8)) == fromInteger stringSeparationCP)
                         || ((x .&. 255) == fromInteger stringSeparationCP)

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
  where digramList = filter (not . stringBoundary) . nonEmptyToList $
                            B.foldl' tally (False, Digram 0, []) txt
        -- tally uses a tuple of 3 values, but the 2 last values could
        -- be gathered in a non-empty list.  Either way, using a
        -- 2-tuple (Bool, [Digram]) results in a compiler warning
        -- because there's no proof that the second element will never
        -- be an empty list.  By making the non-empty list explicit,
        -- the compiler can see that the case analysis is exhaustive.
        tally :: (Bool, Digram, [Digram]) -> Codepoint -> (Bool, Digram, [Digram])
        tally (riskOverlap, hd, tl) c =
          let newDigram = shiftDigram c hd in
           if riskOverlap && (newDigram == hd)
           then (False, hd, tl)
           else (True, newDigram, hd:tl)
        -- make a proper list from the non-empty list represented by
        -- the last 2 elements of tally's state:
        nonEmptyToList :: (a, b, [b]) -> [b]
        nonEmptyToList (_, hd, tl) = hd:tl

foldEnumArray :: Ix a => ((a, b) -> c -> c) -> c -> Array a b -> c
foldEnumArray fun base arr =
    foldl' combine base (range $ bounds arr)
  where combine xs idx = fun (idx, (arr ! idx)) xs

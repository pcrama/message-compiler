module Main

where

import Data.Ix (Ix, range)
import Data.Array ((!), Array, accumArray, array, bounds)
import Data.Char (ord, chr)
import Data.List (sortBy)
import Data.Map (Map, fromListWithKey, size, toList)

import Digram
import EnnGram
import InputText
import Reader

data FullEnnGram = FullEnnGram EnnGram InputText

instance Show FullEnnGram where
  show (FullEnnGram ng it) = runReader (ennString ng) it

instance Eq FullEnnGram where
  f == g = (compare f g) == EQ

instance Ord FullEnnGram where
  compare (FullEnnGram s e) (FullEnnGram t f)
    | sLen > tLen = GT
    | sLen < tLen = LT
    | sLen == tLen =
        foldr cpComp EQ $ take (fromIntegral sLen) $ zip [(ennOffs s)..] [(ennOffs t)..]
    where sLen = ennLength s
          tLen = ennLength t
          cpComp (sIdx, tIdx) res =
              let sCp = e ! sIdx
                  tCp = f ! tIdx
                  comp = compare sCp tCp
              in case comp of
                   EQ -> res
                   otherwise -> comp

-- CombineState2 contains the offset where a new substring
-- with the same content can be counted (index of first
-- char after substring) and the count so far
newtype CombineState2 = CS2 (Offset, Int)
  deriving Show

-- Prevent overlapping instances of substrings: "ababa"
-- shouldn't be counted as containing the substring "aba"
-- twice as for compression purposes, when it's replaced
-- once, the remainder is a two letter string so the same
-- replacement can't be done again.
--
-- Note that `next' and `prev' are swapped because they
-- correspond to the relative positions of the EnnGrams
-- in the input and the list of EnnGrams to count (after
-- filtering for overlap) is built in reverse order, i.e.
-- the EnnGrams last in the input are the first in the list.
--
-- combine2 will be called by fromListWithKey and as such
-- should take its parameters in this order (old means the
-- value that was already in the map and needs to be
-- combined with the new value).  The key is the same
-- substring for both values obviously but its offset
-- in the input text corresponds to the new value.
-- combine2 :: {Key} -> {New Value} -> {Old Value} -> {Combined Value}
combine2 :: FullEnnGram -> CombineState2 -> CombineState2
         -> CombineState2
combine2 f@(FullEnnGram ng _)
         prev@(CS2 (prev_o, prev_c))
         next@(CS2 (next_o, next_c)) =
  if prev_o <= (next_o - (fromIntegral $ ennLength ng))
  then CS2 (prev_o, next_c + 1)
  else next

-- Test case: stringList = ["ababa", "aba", "tab", "aba"]
-- -> at one time during development, the output included an
-- EnnGram "ba\0" (or "\0ba"?), i.e. with a word separator
stringList = ["ababa", "aba", "tab", "aba"]
(Just txt) = toCodepoints stringList

(mmpp, ddtt) = ennGramMap txt

showEnnGramMap :: Map FullEnnGram CombineState2 -> [String]
showEnnGramMap = map showKeyVal
               . take 64
               . sortBy compareGain
               . filter compresses
               . toList
  where showKeyVal (f, CS2 (_, count)) = show (f, count)
        compareGain x y = (compare `on` compressionGain) y x
        compressionGain (FullEnnGram ng _, CS2 (_, count)) =
          (count - 1) * (fromIntegral (ennLength ng) - 1) - 2
        compresses (FullEnnGram ng _, CS2 (_, count)) =
          (ennLength ng == 3) && (count > 2)
          || (ennLength ng > 3) && (count > 1)
        -- found `on' with Hoogle but not in Hugs?
        on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
        on f g x y = f (g x) (g y)

ennGramMap :: InputText -> (Map FullEnnGram CombineState2, DigramTable)
ennGramMap txt = (mp, dt)
  where dt = digramTable txt
        mapfun :: InputText -> (EnnGram, CombineState2)
                  -> (FullEnnGram, CombineState2)
        mapfun txt (ng, cs2) = (FullEnnGram ng txt, cs2)
        mp = fromListWithKey combine2
           $ map (mapfun txt)
           $ makeEnnGramList txt dt

-- Make a list of EnnGram that could possibly appear
-- several times in the InputText.  Each EnnGram is
-- paired with CombineState2 to detect overlaps and
-- prepare for counting them.
--
-- TODO: refactor output type to [EnnGram] because the
-- overlap information is implicit in each EnnGram.
makeEnnGramList :: InputText -> DigramTable
                -> [(EnnGram, CombineState2)]
makeEnnGramList txt dt = el
  where (_, _, el) = foldEnumArray (combine1 txt dt)
                                   (0::Offset, initDigram, [])
                                   txt

-- CombineState1:
-- - Offset: all substrings to prepend to the tail should
--       start from this offset
-- - Digram: last seen Digram (each Digram in a substring
--       is an upper bound for the possible number of
--       occurences of that substring)
-- - List of (EnnGram, CombineState2) pairs: these EnnGrams
--       all have a chance to occur at least twice (i.e.
--       each Digram they contain appears at least twice)
type CombineState1 = (Offset, Digram, [(EnnGram, CombineState2)])

-- combine1 generates a list of substrings (and their
-- positions to filter for overlap) by appending 1 codepoint
-- at a given offset to all substrings currently under
-- consideration.  There are 3 cases:
-- [1] Warmup: the substrings under consideration need to
--     be 3 chars or longer so if the range [prOffs, offs]
--     is shorter than that, we just update the Digram
--     inpTxt = "...0123456789..."
--       cp = 0x36 = '6' ^^
--                prOffs /\ offs
-- [2] Restart: if any digram in the substrings was seen
--     less than twice, then all these substrings can also
--     only occur at most once, which means they aren't
--     interesting compression candidates and extending them
--     with more characters after that makes no sense either
--     so we restart from the current character
-- [3] Enqueue: if the total length is 3 or more characters
--     and the new digram occurs two times or more, all
--     current substrings need to be extended by that new
--     codepoint
--     inpTxt = "...0123456789..."
--       cp = 0x39 = '9' ^   ^
--                prOffs /   \ offs
--     Needs enqueuing 56789, 6789 and 789 (assumes that 89
--     occurs at least twice.
combine1 :: InputText -> DigramTable ->
            (Offset, Codepoint) -> CombineState1 ->
            CombineState1
combine1 inpTxt digTab (offs, cp) prev@(prOffs, prDigram, prTail)
    | stringBoundary newDigram = stopAndRestart
    | (offs - prOffs) < 2 = warmup prOffs newDigram prTail
    | (maxOccur > 1) = enqueueNewEnnGrams prOffs newDigram offs prTail
    | otherwise = stopAndRestart
  where newDigram = shiftDigram cp prDigram
        maxOccur = digTab ! newDigram 
        stopAndRestart = restart offs cp prTail
        warmup :: Offset -> Digram -> [(EnnGram, CombineState2)] ->
                  CombineState1
        warmup o d t = (o, d, t)
        restart :: Offset -> Codepoint -> [(EnnGram, CombineState2)] ->
                   CombineState1
        restart o d t = (o, shiftDigram d initDigram, t)

enqueueNewEnnGrams :: Offset -> Digram -> Offset -> [(EnnGram, CombineState2)] ->
                      CombineState1
enqueueNewEnnGrams prOffs d offs t = (prOffs, d, newTail)
  where newTail = foldr fun t ennGram_lengths
        ennGram_lengths = take (offs - prOffs - 1) [(3::Offset)..]
        fun len base = let Just ng = mkEnnGram (offs - len + 1) len in
          (ng, CS2 (offs + 1, 1)):base

foldArray :: Ix a => (b -> c -> c) -> c -> Array a b -> c
foldArray fun base arr =
    myFoldl' combine base (range $ bounds arr)
  where combine xs idx = fun (arr ! idx) xs

foldEnumArray :: Ix a => ((a, b) -> c -> c) -> c -> Array a b -> c
foldEnumArray fun base arr =
    myFoldl' combine base (range $ bounds arr)
  where combine xs idx = fun (idx, (arr ! idx)) xs

myFoldl' :: (a -> b -> a) -> a -> [b] -> a
myFoldl' _ a [] = a
myFoldl' f a (x:xs) = let fax = f a x in fax `seq` myFoldl' f fax xs

readLicense :: IO InputText
readLicense = do
  theText <- readFile "LICENSE"
  let Just txt = toCodepoints $ lines theText
  return txt

testWithLicense :: IO ()
testWithLicense = readLicense >>= (
  putStrLn . concat . showEnnGramMap . fst . ennGramMap)

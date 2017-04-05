module EnnGram (
  CombineState2(..)
, EnnGramMap
, EnnGram(..)
, ennGramMap
, makeEnnGramList -- internal, exported for testing only
, mkEnnGram
, showEnnGramMap
) where

import qualified Data.ByteString as B
import Data.List (sortBy)
import Data.Array ((!))
import Data.Map (Map, fromListWithKey, toList)
import Data.Char (chr)

import Utils
import InputText
import Digram

-- Not ShortByteString: all EnnGram share the same input string (the
-- complete input text)
newtype EnnGram = EnnGram { fullEnnGramToString :: B.ByteString }

instance Show EnnGram where
  show = map (chr . fromIntegral) . B.unpack . fullEnnGramToString

instance Eq EnnGram where
  f == g = (compare f g) == EQ

instance Ord EnnGram where
  compare (EnnGram s) (EnnGram t)
    -- This isn't lexicographically ordering (we compare on length first) but
    -- any stable/strict ordering will do for Map insertion and looking at the
    -- length first is a bit cheaper (mainly because there is less allocation)
    --
    -- NB: the implementation used to be
    -- | sLen > tLen = GT
    -- | sLen < tLen = LT
    -- | sLen == tLen = compare s t
    -- But (a) the compiler couldn't figure out that the case analysis was
    -- complete and (b) it turned out that the current implementation was
    -- slightly more efficient memory-wise (negligible time impact)
    | sLen == tLen = compare s t
    | otherwise = compare sLen tLen
    where sLen = B.length s
          tLen = B.length t

-- Profiling showed that the original implementation of compare was allocating a lot
-- I tried different implementations with these results:
-- [[ Compiling with ghc -O2 -prof -fprof-auto-top Main.hs ]]
-- [[ Running with Main +RTS -p -RTS ]]
-- Original foldr:
-- FLODR1: 	total time  =        0.66 secs   (659 ticks @ 1000 us, 1 processor)
-- FLODR1: 	total alloc =  31,171,348 bytes  (excludes profiling overheads)
-- FLODR1: COST CENTRE        MODULE             %time %alloc
-- FLODR1: compare            EnnGram             51.0   53.8
-- FLODR1: ennGramMap         EnnGram             25.9   21.9
-- FLODR1:  instance Ord EnnGram where
-- FLODR1:    compare (EnnGram s e) (EnnGram t f)
-- FLODR1:      | sLen > tLen = GT
-- FLODR1:      | sLen < tLen = LT
-- FLODR1:      | sLen == tLen =
-- FLODR1:          foldr cpComp EQ $ take (fromIntegral sLen) $ zip [(ennOffs s)..] [(ennOffs t)..]
-- FLODR1:      where sLen = ennLength s
-- FLODR1:            tLen = ennLength t
-- FLODR1:            cpComp (sIdx, tIdx) res =
-- FLODR1:                let sCp = e ! sIdx
-- FLODR1:                    tCp = f ! tIdx
-- FLODR1:                    comp = compare sCp tCp
-- FLODR1:                in case comp of
-- FLODR1:                     EQ -> res
-- FLODR1:                     otherwise -> comp
-- With Data.List.foldl'
-- FOLDL': 	total time  =        2.17 secs   (2169 ticks @ 1000 us, 1 processor)
-- FOLDL': 	total alloc = 165,408,080 bytes  (excludes profiling overheads)
-- FOLDL': COST CENTRE        MODULE             %time %alloc
-- FOLDL': compare            EnnGram             83.9   91.3
-- FOLDL': ennGramMap         EnnGram              9.3    4.1
-- FOLDL': makeCandidates     CandidateSelection   2.4    2.3
-- FOLDL': enqueueNewEnnGrams EnnGram              1.2    0.6
-- FOLDL':  instance Ord EnnGram where
-- FOLDL':    compare (EnnGram s e) (EnnGram t f)
-- FOLDL':      | sLen > tLen = GT
-- FOLDL':      | sLen < tLen = LT
-- FOLDL':      | sLen == tLen = foldl' cpComp EQ $ take (fromIntegral sLen) $ zip [(ennOffs s)..] [(ennOffs t)..]
-- FOLDL':      where sLen = ennLength s
-- FOLDL':            tLen = ennLength t
-- FOLDL':            cpComp res (sIdx, tIdx) =
-- FOLDL':                let sCp = e ! sIdx
-- FOLDL':                    tCp = f ! tIdx
-- FOLDL':                    comp = compare sCp tCp
-- FOLDL':                in case comp of
-- FOLDL':                     EQ -> res
-- FOLDL':                     otherwise -> comp
-- Using direct recursion (to avoid consing up the list of indices to visit)
-- RECURS: 	total time  =        0.45 secs   (449 ticks @ 1000 us, 1 processor)
-- RECURS: 	total alloc =  14,414,360 bytes  (excludes profiling overheads)
-- RECURS: COST CENTRE        MODULE             %time %alloc
-- RECURS: ennGramMap         EnnGram             32.5   47.4
-- RECURS: compare            EnnGram             27.4    0.0
-- RECURS: makeCandidates     CandidateSelection  11.4   25.8
-- RECURS:  instance Ord EnnGram where
-- RECURS:    compare (EnnGram s e) (EnnGram t f)
-- RECURS:      | sLen > tLen = GT
-- RECURS:      | sLen < tLen = LT
-- RECURS:      | sLen == tLen = go 0 (ennOffs s) (ennOffs t)
-- RECURS:      where sLen = ennLength s
-- RECURS:            tLen = ennLength t
-- RECURS:            go idx sOffs tOffs | idx >= sLen = EQ
-- RECURS:                               | otherwise = let sCp = e ! sOffs
-- RECURS:                                                 tCp = f ! tOffs
-- RECURS:                                                 comp = compare sCp tCp
-- RECURS:                                               in case comp of
-- RECURS:                                                      EQ -> go (idx + 1) (sOffs + 1) (tOffs + 1)
-- RECURS:                                                      otherwise -> comp
-- Final version (using foldr again but consing up a smaller structure)
-- FOLDR2: 	total time  =        0.41 secs   (409 ticks @ 1000 us, 1 processor)
-- FOLDR2: 	total alloc =  14,414,360 bytes  (excludes profiling overheads)
-- FOLDR2: COST CENTRE        MODULE             %time %alloc
-- FOLDR2: ennGramMap         EnnGram             36.4   47.4
-- FOLDR2: compare            EnnGram             28.1    0.0
-- FOLDR2: makeCandidates     CandidateSelection  14.4   25.8
-- FOLDR2: enqueueNewEnnGrams EnnGram              4.9    7.4
-- FOLDR2:  instance Ord EnnGram where
-- FOLDR2:    compare (EnnGram s e) (EnnGram t f)
-- FOLDR2:      | sLen > tLen = GT
-- FOLDR2:      | sLen < tLen = LT
-- FOLDR2:      | sLen == tLen = foldr cpComp EQ [0..fromIntegral sLen - 1]
-- FOLDR2:      where sLen = ennLength s
-- FOLDR2:            tLen = ennLength t
-- FOLDR2:            sIdx = ennOffs s
-- FOLDR2:            tIdx = ennOffs t
-- FOLDR2:            cpComp offs res =
-- FOLDR2:                let sCp = e ! (sIdx + offs)
-- FOLDR2:                    tCp = f ! (tIdx + offs)
-- FOLDR2:                    comp = compare sCp tCp
-- FOLDR2:                in case comp of
-- FOLDR2:                     EQ -> res
-- FOLDR2:                     otherwise -> comp

mkEnnGram :: InputText -> Offset -> Length -> Maybe EnnGram
mkEnnGram it offs len =
  if (len > 2)
  then Just . EnnGram $ B.take len $ B.drop offs it
  else Nothing

unsafeMkEnnGram :: InputText -> Offset -> Length -> EnnGram
unsafeMkEnnGram it offs len = EnnGram $ B.take len $ B.drop offs it

type EnnGramMap = Map EnnGram CombineState2

showEnnGramMap :: EnnGramMap -> [String]
showEnnGramMap = map showKeyVal
               . take 64
               . sortBy compareGain
               . filter compresses
               . toList
  where showKeyVal (f, CS2 (_, count)) = show (f, count)
        compareGain x y = (compare `on` compressionGain') y x
        compressionGain' (EnnGram ng, CS2 (_, count)) =
          compressionGain (B.length ng) count
        compresses (EnnGram ng, CS2 (_, count)) =
          (B.length ng == 3) && (count > 2)
          || (B.length ng > 3) && (count > 1)

ennGramMap :: InputText -> (EnnGramMap, DigramTable)
ennGramMap txt = (mp, dt)
  where dt = digramTable txt
        mp = fromListWithKey combine2
           $ makeEnnGramList txt dt

-- Make a list of EnnGram that could possibly appear
-- several times in the InputText.  Each EnnGram is
-- paired with CombineState2 to detect overlaps and
-- prepare for counting them.
--
-- Can't refactor output type to only [EnnGram] even though
-- the overlap information is implicit in each EnnGram:
-- when inserting a new EnnGram in the Map, function
-- combining the old value in the map with the new one
-- only gets the new key but can't recover the old key
-- (same string, but at a different offset) easily.
makeEnnGramList :: InputText -> DigramTable
                -> [(EnnGram, CombineState2)]
makeEnnGramList txt dt = el
  where (_, _, el) = foldEnumByteString (combine1 txt dt)
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
--     is shorter than that, we update the Digram
--     [a] Digram is incomplete (i.e. only one Char read,
--         offs == prOffs): accept updated Digram and
--         continue immediately: we don't have enough
--         information yet to do more.
--     [b] Digram is complete (i.e. offs - prOffs == 1)
--     inpTxt = "...0123456789..."
--       cp = 0x36 = '6' ^^       newDigram="56"
--                prOffs /\ offs
--         If that Digram occurs > 1, future EnnGram starting
--         with it might also occur several times, so
--         accept new Digram and continue immediately.
--         If that Digram only occurs once, all following
--         EnnGram will also only occur once, so restart
--         from current char (i.e. at offs)
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
combine1 :: InputText -> DigramTable -> (Offset, Codepoint) -> CombineState1 ->
            CombineState1
combine1 it digTab (offs, cp) (prOffs, prDigram, prTail)
    | (((offs - prOffs) == 1 && (maxOccur > 1))
       || (offs == prOffs)) =
       warmup prOffs newDigram prTail
    | (maxOccur > 1) = enqueueNewEnnGrams it maxOccur prOffs newDigram offs prTail
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

enqueueNewEnnGrams :: InputText -> Int -> Offset -> Digram -> Offset -> [(EnnGram, CombineState2)] ->
                      CombineState1
enqueueNewEnnGrams it maxOccur prOffs d offs t = (prOffs, d, newTail)
  where newTail = foldr fun t ennGramLengths
        ennGramLengths = filter (\len -> compressionGain len maxOccur > 0)
                         $ take (offs - prOffs - 1) [(3::Length)..]
        -- by construction (see the 3::Length starting index in ennGramLengths)
        -- the length is always > 2, so we may call the unsafe version, and
        -- produce less garbage.
        fun len base =
            (unsafeMkEnnGram it (offs - len + 1) len, CS2 (offs + 1, 1)):base

-- CombineState2 contains the offset where a new substring
-- with the same content can be counted (index of first
-- char after substring) and the count so far
newtype CombineState2 = CS2 (Offset, Count)
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
combine2 :: EnnGram -> CombineState2 -> CombineState2
         -> CombineState2
combine2 (EnnGram ng)
         (CS2 (prev_o, _prev_c))
         next@(CS2 (next_o, next_c)) =
  if prev_o <= (next_o - (fromIntegral $ B.length ng))
  then CS2 (prev_o, next_c + 1)
  else next

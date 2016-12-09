module Main

where

import Data.Ix (Ix, range)
import Data.Array ((!), Array, accumArray, array, bounds)
import Data.Char (ord, chr)
import Data.List (sortBy, unfoldr, foldl')
import Data.Map (Map, fromListWithKey, size, toList)

import Utils
import Digram
import EnnGram
import InputText
import Reader

bestCandidate :: EnnGramMap -> DigramTable -> Maybe (String, Int)
bestCandidate mp dt = result
  where bestE :: [(FullEnnGram, CombineState2)]
        bestE = take 1
              . sortBy compareEnnGain
              . filter ennCompresses
              $ toList mp
        filterDigram :: Int -> Bool
        filterDigram =
          case bestE of
            [(FullEnnGram e _, CS2 (_, ec))] ->
                let eg = compressionGain (ennLength e) ec
                in \c -> compressionGain 2 c > eg
            [] ->
                \c -> c >= minCountLen2
        bestD = take 1
              $ foldEnumArray
                  (\nw@(_, newC) xs ->
                    let acceptable = filterDigram newC
                        improves = case xs of
                                     [] -> True
                                     ((oldDi, oldC):_) -> 
                                         newC > oldC
                    in if acceptable && improves
                       then nw:xs
                       else xs)
                  []
                  dt
        result = case bestD of
                   [(digram, c)] ->
                       Just (unDigram digram, c)
                   [] ->
                       case bestE of
                         [(fe, CS2 (_, ec))] ->
                             Just (fullEnnGramToString fe
                                  ,ec)
                         [] -> Nothing
        compareEnnGain x y = (compare `on` compressionGain') y x
        compressionGain' (FullEnnGram ng _, CS2 (_, count)) =
          compressionGain (ennLength ng) count
        ennCompresses (FullEnnGram ng _, CS2 (_, count)) =
          (ennLength ng == 3) && (count >= minCountLen3)
          || (ennLength ng > 3) && (count > minCountLen4)
        -- found `on' with Hoogle but not in Hugs?
        on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
        on f g x y = f (g x) (g y)

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

type EnnGramMap = Map FullEnnGram CombineState2

showEnnGramMap :: EnnGramMap -> [String]
showEnnGramMap = map showKeyVal
               . take 64
               . sortBy compareGain
               . filter compresses
               . toList
  where showKeyVal (f, CS2 (_, count)) = show (f, count)
        compareGain x y = (compare `on` compressionGain') y x
        compressionGain' (FullEnnGram ng _, CS2 (_, count)) =
          compressionGain (ennLength ng) count
        compresses (FullEnnGram ng _, CS2 (_, count)) =
          (ennLength ng == 3) && (count > 2)
          || (ennLength ng > 3) && (count > 1)
        -- found `on' with Hoogle but not in Hugs?
        on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
        on f g x y = f (g x) (g y)

ennGramMap :: InputText -> (EnnGramMap, DigramTable)
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
-- Can't refactor output type to only [EnnGram] even though
-- the overlap information is implicit in each EnnGram:
-- when inserting a new EnnGram in the Map, function
-- combining the old value in the map with the new one
-- only gets the new key but can't recover the old key
-- (same string, but at a different offset) easily.
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
combine1 :: InputText -> DigramTable ->
            (Offset, Codepoint) -> CombineState1 ->
            CombineState1
combine1 inpTxt digTab (offs, cp) prev@(prOffs, prDigram, prTail)
    | (((offs - prOffs) == 1 && (maxOccur > 1))
       || (offs == prOffs)) =
       warmup prOffs newDigram prTail
    | (maxOccur > 1) = enqueueNewEnnGrams maxOccur prOffs newDigram offs prTail
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

enqueueNewEnnGrams :: Int -> Offset -> Digram -> Offset -> [(EnnGram, CombineState2)] ->
                      CombineState1
enqueueNewEnnGrams maxOccur prOffs d offs t = (prOffs, d, newTail)
  where newTail = foldr fun t ennGramLengths
        ennGramLengths = filter (\len -> compressionGain len maxOccur > 0)
                         $ take (offs - prOffs - 1) [(3::Length)..]
        fun len base = let Just ng = mkEnnGram (offs - len + 1) len in
          (ng, CS2 (offs + 1, 1)):base

readLicense :: IO InputText
readLicense = do
  theText <- readFile "LICENSE"
  let Just txt = toCodepoints $ lines theText
  return txt

testWithLicense :: IO ()
testWithLicense = readLicense >>= (
  putStrLn . concat . showEnnGramMap . fst . ennGramMap)

firstComprCand :: IO ()
firstComprCand = readLicense >>= (
  putStrLn . show . uncurry bestCandidate . ennGramMap)

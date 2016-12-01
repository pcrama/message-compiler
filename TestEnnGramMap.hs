module TestEnnGramMap where

import Data.List (sortBy)
import qualified Data.Map as M
import Debug.Trace (trace)

import Digram
import EnnGram
import InputText
import Main

--traceShow x = trace ( show x) x
traceShow = id

oneTestMakeEnnGramList :: [String] -> [(Offset, Length)]
                       -> Either ([String], [(EnnGram, CombineState2)]) Bool
oneTestMakeEnnGramList sl expected =
    if length sorted == length expected
    && (and $ zipWith (==)
                      (map (toTuple . fst) sorted)
                      $ map toTuple . sortBy ennGramCompare
                        $ makeExpectedEnnGramList expected)
    then Right True
    else Left (sl, sorted)
  where Just it = toCodepoints (traceShow sl)
        dt = digramTable it
        eglist = makeEnnGramList it dt
        sorted = sortBy ennGramCs2compare eglist
        ennGramCs2compare = ennGramCompare `on` fst
        ennGramCompare ng1 ng2
          | ennOffs ng1 == ennOffs ng2 = (compare `on` ennLength) ng1 ng2
          | otherwise = (compare `on` ennOffs) ng1 ng2
        on f g x = f (g x) . g
        toTuple ng = (ennOffs ng, ennLength ng)
          
-- I don't have traverse in Hugs :-(, otherwise, I'd
-- write maybe [] id . traverse . map mkEnnGram
makeExpectedEnnGramList =
  foldr (\(offs, len) acc ->
             maybe [] (:acc) $ mkEnnGram (offs :: Offset) len)
        []

-- No Monad instance for Either in Hugs? I'll just fake one
-- here...
instance Monad (Either a) where
  return = Right
  (Left x) >>= _ = Left x
  (Right y) >>= k = k y

testMakeEnnGramList :: Either ([String], [(EnnGram, CombineState2)]) Bool
testMakeEnnGramList = do
  oneTestMakeEnnGramList [] []
  -- no EnnGram in input implies there are none in output
  oneTestMakeEnnGramList ["11", "11", "11"] []
  -- no EnnGram of length 3 should be generated if there
  -- can be at most two of them (the Digram "22" appears
  -- only twice
  oneTestMakeEnnGramList ["222", "222"] []
  -- no EnnGram of length 3 should be generated if there
  -- can be at most two of them (the Digram "22" appears
  -- only twice, but EnnGram of length > 3 occuring at most
  -- twice is OK
  oneTestMakeEnnGramList ["abcd", "abcd"] [(0,4),(5,4)]
  oneTestMakeEnnGramList ["abcd", "abcde", "bacd"]
                         [(0,4),(5,4)
                         -- ideally, these (bcd) wouldn't be
                         -- generated but only one Digram
                         -- at a time is considered, so
                         -- when "cd" (3 occurences) is seen,
                         -- "bcd" is generated even though
                         -- "bc" only occurs twice and an
                         -- EnnGram of length 3 only gains
                         -- when it occure more often.
                         ,(1,3),(6,3)
                         ]
  -- some time during development, the output included an
  -- EnnGram "ba\0" (or "\0ba"?), i.e. with a word separator
  oneTestMakeEnnGramList ["ababa", "aba", "tab", "aba"] 
                         [(0,3),(0,4),(0,5)
                         ,(1,3),(1,4)
                         ,(2,3)
                         ,(6,3)
                         ,(14,3)]
  -- This test case added to check that skipping upon seeing
  -- `ta' (Digram occuring only once) doesn't miss the `aba'
  -- that follows.  I.e. skipping because of a Digram doesn't
  -- forget about the second char of the Digram
  oneTestMakeEnnGramList ["ababa", "aba", "taba"] 
                         [(0,3),(0,4),(0,5)
                         ,(1,3),(1,4)
                         ,(2,3)
                         ,(6,3)
                         ,(11,3)]
  oneTestMakeEnnGramList 
    ["aaaaaa","aaa"]
    [(0,3),(0,4),(0,5),(0,6)
    ,(1,3),(1,4),(1,5)
    ,(2,3),(2,4)
    ,(3,3)
    ,(7,3)]
  oneTestMakeEnnGramList 
    ["aaaaaaa","aaa"]
    [(0,3),(0,4),(0,5),(0,6),(0,7)
    ,(1,3),(1,4),(1,5),(1,6)
    ,(2,3),(2,4),(2,5)
    ,(3,3),(3,4)
    ,(4,3)
    ,(8,3)]

oneTestMakeEnnGramMap :: [String] -> [(Offset,Length,Int)]
                      -> Either ([String]
                                , M.Map FullEnnGram CombineState2)
                                Bool
oneTestMakeEnnGramMap sl exp
  | (length exp == M.size mp)
    && (all ennGramExistsAndCountMatches exp)
    = Right True
  | otherwise = Left (sl, mp)
  where Just it = toCodepoints (traceShow sl)
        (mp, dt) = ennGramMap it
        ennGramExistsAndCountMatches (offs, len, count) =
            case M.lookup key mp of
              Just (CS2 (_, c)) -> c == count
              Nothing -> False
          where key = FullEnnGram ng it
                Just ng = mkEnnGram offs len

testMakeEnnGramMap :: Either ([String]
                              , M.Map FullEnnGram CombineState2)
                             Bool
testMakeEnnGramMap = do
  oneTestMakeEnnGramMap [] []
  oneTestMakeEnnGramMap ["", "", "", ""] []
  oneTestMakeEnnGramMap ["abc", "", "", ""] []
  -- substrings too short to give EnnGram
  oneTestMakeEnnGramMap ["ab", "ab", "ab", "ab"] []
  -- occurence 1 each -> no use to put in the map
  oneTestMakeEnnGramMap ["abc", "def", "", ""] []
  -- EnnGram of length 3 and occurence = 2 give no gain
  -- -> not included in map
  oneTestMakeEnnGramMap ["abc", "abc", "", ""] []
  oneTestMakeEnnGramMap ["abcd", "abc", "", ""] []
  oneTestMakeEnnGramMap ["abcd", "abcd", "", ""] [(0, 4, 2)]
  -- unfortunately, only the last Digram occurence count is
  -- used as approximation (upper bound) of the EnnGram count
  -- `bc' occurs thrice, hence `abc' is included even though
  -- in the end, it occurs only twice.
  --
  -- Do the extra useless entries in the map hurt more than
  -- the extra complexity to get this 100% right?
  oneTestMakeEnnGramMap ["abcd", "abcd", "bcd"]
                        [(0, 3, 2), (1, 3, 3), (0, 4, 2)]
  let s1 = "This is his"
  let s2 = "This is hers"
  -- Expected result for [s1, s2].  See `juggle' below to
  -- transform it to the expected result for [s2, s1]
  let exp = [
         (0,3,2)
        ,(0,4,2),(0,5,2),(0,6,2),(0,7,2),(0,8,2),(0,9,2)
        -- Even though each Digram occurs >=2, the complete
        -- substring occurs only once.
        ,(0,10,1),(0,11,1)
        ,(1,3,3),(1,4,2),(1,5,2),(1,6,2),(1,7,2),(1,8,2)
        ,(1,9,1),(1,10,1)
        ,(2,3,4),(2,4,2),(2,5,2),(2,6,2),(2,7,2)
        ,(2,8,1),(2,9,1)
        -- (3,3,2) avoided because " i" only occurs twice
        ,(3,4,2),(3,5,2),(3,6,2)
        ,(3,7,1),(3,8,1)
        ,(4,3,2)
        ,(4,4,2),(4,5,2)
        ,(4,6,1),(4,7,1)
        -- (5,3,1), is in (2,3,4)
        ,(5,4,2)
        ,(5,5,1),(5,6,1)
        -- (6,3,2) avoided because " h" only occurs twice
        ,(6,4,1),(6,5,1)
        ,(7,3,1),(7,4,1)
        -- (8,3,1) in (1,3,3)
        -- (12,3,1)..(12,9,1) already in (0,3,2)..(0,9,2)
        -- (12,10,1) to end of string supressed because hers
        -- consists of Digram occuring only once
        -- similar for all other substrings
        ]
  oneTestMakeEnnGramMap [s1, s2] exp
  -- order of strings in input shouldn't matter but we have
  -- to fix the indices of the substrings
  let juggle (s, l, c) = (if s > length s1
                          then s - length s1 - 1
                          else s + length s2 + 1
                         , l
                         , c)
  oneTestMakeEnnGramMap [s2, s1]
                      $ map juggle exp

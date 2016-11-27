module TestEnnGramMap where

import Data.List (sortBy)
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


eeer = do
  oneTestMakeEnnGramList ["ababa", "aba", "taba"] 
                         [(0,3),(0,4),(0,5)
                         ,(1,3),(1,4)
                         ,(2,3)
                         ,(6,3)
                         -- why `tab'? --,(10,3)
                         ,(11,3)]

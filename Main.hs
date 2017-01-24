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
import CandidateSelection

-- Test case: stringList = ["ababa", "aba", "tab", "aba"]
-- -> at one time during development, the output included an
-- EnnGram "ba\0" (or "\0ba"?), i.e. with a word separator
stringList = ["ababa", "aba", "tab", "aba"]
(Just txt) = toCodepoints stringList

(mmpp, ddtt) = ennGramMap txt

readLicense :: IO InputText
readLicense = do
  theText <- readFile "LICENSE"
  let Just txt = toCodepoints $ lines theText
  return txt

testWithLicense :: IO ()
testWithLicense = readLicense >>= (
  putStrLn . concat . showEnnGramMap . fst . ennGramMap)

firstComprBatch :: IO ()
firstComprBatch = readLicense >>= (
  putStrLn . show . getNextCandidates . uncurry makeCandidates . ennGramMap)

main = firstComprBatch

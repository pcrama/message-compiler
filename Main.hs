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

module Main

where

import Data.Ix (Ix, range)
import Data.Array ((!), Array, accumArray, array, bounds)
import Data.Char (ord, chr)
import Data.List (sortBy, unfoldr, foldl')
import Data.Map (Map, fromListWithKey, size, toList)
import qualified Data.ByteString as B

import Utils
import Digram
import EnnGram
import InputText
import CandidateSelection
import Compression

readLicense :: IO InputText
readLicense = do
  theText <- readFile "LICENSE"
  let Just txt = toCodepoints $ lines theText
  return txt

testWithLicense :: IO ()
testWithLicense = readLicense >>= (
  putStrLn . concat . showEnnGramMap . fst . ennGramMap)

firstComprBatch :: IO ()
firstComprBatch = do
  lic <- readLicense
  let cands = getNextCandidates . uncurry makeCandidates $ ennGramMap lic
  B.putStr $ applyCompression lic $ zip cands [192..]

doCompr :: [Codepoint] -> InputText -> InputText
doCompr [] txt = txt
doCompr replacements txt =
  let cands = getNextCandidates . uncurry makeCandidates $ ennGramMap txt
      (rep1, rep2) = splitAt (length cands) replacements
      compressed = applyCompression txt $ zip cands rep1
  in case cands of
       [] -> txt
       otherwise -> doCompr rep2 compressed

twoComprSteps :: IO ()
twoComprSteps = do
  lic <- readLicense
  let replacements = take maxCompressions $ [192..]
  let cands1 = getNextCandidates . uncurry makeCandidates $ ennGramMap lic
  let (rep1, rep2) = splitAt (length cands1) replacements
  let comp1 = applyCompression lic $ zip cands1 rep1
  let cands2 = getNextCandidates . uncurry makeCandidates $ ennGramMap comp1
  let comp2 = applyCompression comp1 $ zip cands2 rep2
  B.putStr $ comp2

allComprSteps :: IO ()
allComprSteps = do
  lic <- readLicense
  let replacements = take maxCompressions $ [192..]
  B.putStr $ doCompr replacements lic

main = allComprSteps

module Main

where

import Data.Char (ord, chr)
import qualified Data.ByteString as B

import Utils
import EnnGram
import InputText
import CandidateSelection
import Compression
import TopFunctions

readLicense :: IO InputText
readLicense = do
  theText <- readFile "LICENSE"
  -- using `lines' loses information whether `theText' finished with
  -- '\n' or not: lines "a\nb" == lines "a\nb\n"
  let Just txt = toCodepoints $ lines theText
  return txt

testWithLicense :: IO ()
testWithLicense = readLicense >>= (
  putStrLn . concat . showEnnGramMap . fst . ennGramMap)

firstComprBatch :: IO ()
firstComprBatch = do
  lic <- readLicense
  let cands = getNextCandidates . uncurry makeCandidates $ ennGramMap lic
  B.putStr $ applyCompression lic $ zip cands [fromIntegral firstCompressionMarker..]

twoComprSteps :: IO ()
twoComprSteps = do
  lic <- readLicense
  let replacements = take maxCompressions $ [fromIntegral firstCompressionMarker..]
  let cands1 = getNextCandidates . uncurry makeCandidates $ ennGramMap lic
  let (rep1, rep2) = splitAt (length cands1) replacements
  let comp1 = applyCompression lic $ zip cands1 rep1
  let cands2 = getNextCandidates . uncurry makeCandidates $ ennGramMap comp1
  let comp2 = applyCompression comp1 $ zip cands2 rep2
  B.putStr $ comp2

allComprSteps :: IO ()
allComprSteps = do
  lic <- readFile "LICENSE"
  let mbAssCmpr = compressText lic
  flip (maybe (return ())) mbAssCmpr $ \(cpAssoc, compressed) ->
    let decompressed = decompressText (cpAssoc, compressed)
    in do
         B.putStr $ compressed
         B.putStr $ B.singleton 10
         B.putStr $ decompressed
         B.putStr $ B.singleton 10
         -- compare on `lines' to avoid ambiguity of separator between
         -- text and substrings
         putStrLn . show $ lines (b2s decompressed) == lines lic

s2b :: String -> B.ByteString
s2b = B.pack . map (fromIntegral . ord)

b2s :: B.ByteString -> String
b2s = map (chr . fromIntegral) . B.unpack

propertyCompressionIsReversible :: String -> Bool
propertyCompressionIsReversible s = maybe False decompressAndCheck $ compressText s
  where decompressAndCheck z = equalityUpToTrailingNL (decompressText z)
                                                      (B.pack $ map (fromIntegral . ord) s)
        equalityUpToTrailingNL a b =
          let lenA = B.length a
              lenB = B.length b
              comp longer shorter shortLen =
                let (hd, tl) = B.splitAt shortLen longer
                in shorter == hd && tl == B.singleton 10
          in case compare lenA lenB of
               EQ -> a == b
               GT -> comp a b lenB
               LT -> comp b a lenA

propertyCompressionSavesSpace :: String -> Bool
propertyCompressionSavesSpace s = maybe False savesSpace $ compressText s
  where savesSpace (cpAssoc, compressed) =
           (sum $ map ((1+) . B.length . snd) cpAssoc) + B.length compressed
         < length s

propertyAllCompressionsUsed :: String -> Bool
propertyAllCompressionsUsed = maybe False allUsed . compressText
  where allUsed (cpAssoc, compressed) = all (inCompressedOrSubstrings compressed cpAssoc) cpAssoc
        inCompressedOrSubstrings compressed cpAssoc (cp, _) =
          B.elem cp compressed || (any (B.elem cp . snd)
                                     $ filter ((/= cp) . fst) cpAssoc)

main :: IO ()
main = allComprSteps

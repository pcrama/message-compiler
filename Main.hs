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
  B.putStr $ applyCompression lic $ zip cands [fromIntegral firstCompressionMarker..]

doCompr :: [Codepoint] -> InputText -> ([(Codepoint, B.ByteString)], InputText)
doCompr cps it = cutOffCandidates . sortAssocList $ go [] cps it
  where go :: [(Codepoint, B.ByteString)] -> [Codepoint] -> InputText
           -> ([(Codepoint, B.ByteString)], InputText)
        go cpAssoc [] txt = (cpAssoc, txt)
        go cpAssoc replacements txt =
            let cands = getNextCandidates . uncurry makeCandidates $ ennGramMap txt
                (rep1, rep2) = splitAt (length cands) replacements
                compressed = applyCompression txt $ zip cands rep1
            in case cands of
                 [] -> (cpAssoc, txt)
                 otherwise -> go (zipWith (\cp (Candidate bs _) -> (cp, bs))
                                          rep1
                                          cands
                                  ++ cpAssoc)
                                 rep2
                                 compressed
        sortAssocList :: ([(Codepoint, B.ByteString)], InputText)
                      -> ([(Codepoint, B.ByteString)], InputText)
        sortAssocList (cpAssoc, txt) = (sortBy (compare `on` fst) cpAssoc, txt)
        cutOffCandidates :: ([(Codepoint, B.ByteString)], InputText)
                         -> ([(Codepoint, B.ByteString)], InputText)
        cutOffCandidates (lst, it) =
            let candCount = length lst
                (sepIndices, _) = splitAt candCount
                                        $ reverse $ B.findIndices (== stringSeparationCP) it
                comprEnd = last sepIndices
            in ((zip (map fst lst) $ cutUp sepIndices it), fst $ B.splitAt comprEnd it)

-- cut up ByteString in pieces marked by list of indices (assumed to be
-- in decreasing order).  The char at the index is dropped (assumed to be
-- a separator).  The returned list of ByteString is again in the order
-- of the input ByteString (i.e. reverse order of the input list of indices)
cutUp :: [Int] -> B.ByteString -> [B.ByteString]
cutUp revIndices s = fst $ foldl' popOne ([], s) revIndices
  where popOne :: ([B.ByteString], B.ByteString) -> Int -> ([B.ByteString], B.ByteString)
        popOne (res, s) idx = ((B.tail new):res, head)
          where (head, new) = B.splitAt idx s

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

compressText :: String -> Maybe ([(Codepoint, B.ByteString)], InputText)
compressText s = do
  txt <- toCodepoints $ lines s
  let replacements = take maxCompressions $ [fromIntegral firstCompressionMarker..]
  return $ doCompr replacements txt

decompressText :: ([(Codepoint, B.ByteString)], InputText) -> B.ByteString
decompressText (cpAssoc, compressed) = rebuilt
  where decompressed = decompress compressed cpAssoc
        rebuilt = B.intercalate (B.singleton 10) $ B.split stringSeparationCP decompressed

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
         putStrLn . show $ decompressed == B.pack (map (fromIntegral . ord) lic)

s2b = B.pack . map (fromIntegral . ord)

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

main = allComprSteps

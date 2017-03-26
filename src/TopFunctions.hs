module TopFunctions (
    compressText
  , decompressText
)

where

import Data.List (sortBy, foldl')
import qualified Data.ByteString as B

import Utils
import EnnGram
import InputText
import CandidateSelection
import Compression

compressText :: String -> Maybe ([(Codepoint, B.ByteString)], InputText)
compressText s = do
  -- using `lines' loses information whether `s' finished with
  -- '\n' or not: lines "a\nb" == lines "a\nb\n"
  txt <- toCodepoints $ lines s
  let replacements = take maxCompressions $ [fromIntegral firstCompressionMarker..]
  return $ doCompr replacements txt

decompressText :: ([(Codepoint, B.ByteString)], InputText) -> B.ByteString
decompressText (cpAssoc, compressed) = rebuilt
  where decompressed = decompress compressed cpAssoc
        rebuilt = B.intercalate (B.singleton 10) $ B.split stringSeparationCP decompressed

doCompr :: [Codepoint] -> InputText -> ([(Codepoint, B.ByteString)], InputText)
doCompr cps it = cutOffCandidates . sortAssocList $ go [] cps it
  where go :: [(Codepoint, B.ByteString)] -> [Codepoint] -> InputText
           -> ([(Codepoint, B.ByteString)], InputText)
        go cpAssoc [] txt = (cpAssoc, txt)
        go cpAssoc replacements txt =
            let cands = getNextCandidates
                      . uncurry (makeCandidates $ length replacements)
                      $ ennGramMap txt
                (rep1, rep2) = splitAt (length cands) replacements
                compressed = applyCompression txt $ zip cands rep1
            in case cands of
                 [] -> (cpAssoc, txt)
                 _ -> go (   zipWith (\cp (Candidate bs _) -> (cp, bs)) rep1 cands
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
                comprEnd = if null sepIndices then B.length it else last sepIndices
            in ((zip (map fst lst) $ cutUp sepIndices it), fst $ B.splitAt comprEnd it)

-- cut up ByteString in pieces marked by list of indices (assumed to be
-- in decreasing order).  The char at the index is dropped (assumed to be
-- a separator).  The returned list of ByteString is again in the order
-- of the input ByteString (i.e. reverse order of the input list of indices)
cutUp :: [Int] -> B.ByteString -> [B.ByteString]
cutUp revIndices s = fst $ foldl' popOne ([], s) revIndices
  where popOne :: ([B.ByteString], B.ByteString) -> Int -> ([B.ByteString], B.ByteString)
        popOne (res, s) idx = ((B.tail new):res, hd)
          where (hd, new) = B.splitAt idx s

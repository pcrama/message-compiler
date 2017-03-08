module Compression

where

import Data.Ord (comparing)
import Data.ByteString.Builder
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Search as BS
import Data.Array (Array, array, (!))

import InputText
import CandidateSelection

data ReplacementInfo = ReplacementInfo { lenToReplace :: Int -- how many bytes to remove from original
                                       , codepoint :: Codepoint -- what to put in place
                                       , firstPos :: Int -- there's always at least one, otherwise, it wouldn't be a candidate
                                       , positions :: [Int] -- positions where this replacement can be made
                                       }
  deriving (Show, Eq)

decompress :: InputText -> [(Codepoint, B.ByteString)] -> B.ByteString
decompress it cpAssoc = BL.toStrict
                      $ toLazyByteString
                      $ decompressBS it $ decompressionMap cpAssoc

type DecompressionMap = Array Codepoint Builder

-- assume the mapping is sorted by ascending Codepoint
decompressionMap :: [(Codepoint, B.ByteString)] -> DecompressionMap
decompressionMap cpAssoc = all
  where all = array (minBound, maxBound)
                  $ mergePrefer2nd [(x, word8 x) | x <- [minBound..maxBound]] cpAssoc
        recursiveDecompress = flip decompressBS all
        mergePrefer2nd :: [(Codepoint, Builder)] -> [(Codepoint, B.ByteString)] -> [(Codepoint, Builder)]
        mergePrefer2nd [] ys = map (\(cp, bs) -> (cp, recursiveDecompress bs)) ys
        mergePrefer2nd xs [] = xs
        mergePrefer2nd (xx@(x, builderX):xs) yy@((y, bsY):ys)
            | x == y    = (y, recursiveDecompress bsY):mergePrefer2nd xs ys
            | otherwise = xx:mergePrefer2nd xs yy

decompressBS :: B.ByteString -> DecompressionMap -> Builder
decompressBS it cpMap =  B.foldl' (\soFar new -> mappend soFar $ decompressCodePoint cpMap new)
                                  mempty
                                  it

decompressCodePoint :: DecompressionMap -> Codepoint -> Builder
decompressCodePoint cpMap cp = cpMap ! cp

applyCompression :: InputText -> [(Candidate, Codepoint)] -> InputText
applyCompression input cands =
      BL.toStrict
    $ toLazyByteString
    $ (`mappend` foldMap (\(Candidate c _, _) -> word8 0 `mappend` byteString c) cands)
    $ go 0 mempty input $ map (\(Candidate c _, cp) ->
                                  let pos = BS.nonOverlappingIndices c input
                                  in ReplacementInfo { lenToReplace = B.length c
                                                     , codepoint = cp
                                                     -- pos is never null because there are always at least
                                                     -- two matches, otherwise, it wouldn't be a candidate
                                                     , firstPos = head pos
                                                     , positions = tail pos})
                              cands
  where go :: Int -> Builder -> B.ByteString -> [ReplacementInfo] -> Builder
        go offs b input [] = b `mappend` byteString (B.drop offs input)
        go offs b input cands =
            let (nextCandLen, nextCp, otherCands, candOffs) = selectNextCand cands
            in go (candOffs + nextCandLen)
                  (mappend b
                         $ mappend (byteString . B.take (candOffs - offs) . B.drop offs $ input)
                                   (word8 nextCp))
                  input
                  otherCands

selectNextCand :: [ReplacementInfo] -> (Int, Codepoint, [ReplacementInfo], Int)
selectNextCand [] = error "Should never happen: selectNextCand called with empty list"
selectNextCand (replInfo:xs) = improve replInfo xs []
  where improve replInfo [] xs = (lenToReplace replInfo
                                 , codepoint replInfo
                                 , if null (positions replInfo)
                                   then xs
                                   else replInfo { firstPos = head $ positions replInfo
                                                 , positions = tail $ positions replInfo
                                        }:xs
                                 , firstPos replInfo)
        improve curr (next:lcos) xs =
            let preferCurr = improve curr lcos (next:xs)
                preferNext = improve next lcos (curr:xs)
            in case comparing firstPos curr next of
                 LT -> preferCurr -- apply first occurring replacement
                 EQ -> case comparing lenToReplace curr next of -- TODO: is this even necessary?
                         LT -> preferNext -- prefer longer replacements
                         EQ -> error "Should never happen: curr and next can't be the same candidate"
                         GT -> preferCurr
                 GT -> preferNext

module Compression

where

import Data.Ord (comparing)
import Data.ByteString.Builder
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Search as BS

import InputText
import CandidateSelection

data ReplacementInfo = ReplacementInfo { lenToReplace :: Int -- how many bytes to remove from original
                                       , codepoint :: Codepoint -- what to put in place
                                       , firstPos :: Int -- there's always at least one, otherwise, it wouldn't be a candidate
                                       , positions :: [Int] -- positions where this replacement can be made
                                       }
  deriving (Show, Eq)

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

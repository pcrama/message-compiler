module Properties (
    propertyCompressionIsReversible
  , propertyCompressionSavesSpace
  , propertyAllCompressionsUsed
)

where

import Test.QuickCheck

import qualified Data.Bits
import Data.Char (chr, ord)
import qualified Data.ByteString as B

import TopFunctions

newtype InputString = IS { unIS :: String } deriving (Show, Eq)

genInputString :: Gen InputString
genInputString = do
    s <- arbitrary
    return . IS $ map foldAnyCharToInputString s
  where foldAnyCharToInputString c = let ordc = ord c in
          if ordc == 10 || ordc >= 32 && ordc <= 127
          then c
          else chr $ 32 + (ordc Data.Bits..&. 64)

instance Arbitrary InputString where
  arbitrary = genInputString

propertyCompressionIsReversible :: InputString -> Bool
propertyCompressionIsReversible (IS s) = maybe False decompressAndCheck $ compressText s
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

propertyCompressionSavesSpace :: InputString -> Bool
propertyCompressionSavesSpace (IS s) = maybe False savesSpace $ compressText s
  where savesSpace (cpAssoc, compressed) =
           (sum $ map ((1+) . B.length . snd) cpAssoc) + B.length compressed
         < length s

propertyAllCompressionsUsed :: InputString -> Bool
propertyAllCompressionsUsed = maybe False allUsed . compressText . unIS
  where allUsed (cpAssoc, compressed) = all (inCompressedOrSubstrings compressed cpAssoc) cpAssoc
        inCompressedOrSubstrings compressed cpAssoc (cp, _) =
          B.elem cp compressed || (any (B.elem cp . snd)
                                     $ filter ((/= cp) . fst) cpAssoc)

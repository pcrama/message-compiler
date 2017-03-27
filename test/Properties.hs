module Properties (
    propertyCompressionIsReversible
  , propertyCompressionSavesSpaceOrId
  , propertyAllCompressionsUsed
  , propertyGetNextCandidatesEquivalence
  , propertyOverlapEquivalence
)

where

import Test.QuickCheck

import qualified Data.Bits
import Data.Char (chr, ord)
import qualified Data.ByteString as B
import Data.List (nub, sort)

import CandidateSelection (Candidate(..), overlaps, getNextCandidates, getNextDigrams)
import Digram (Digram, unDigram, overlapsDigram, initDigram, shiftDigram)
import TopFunctions
import Utils (Count, minCountLen2, on)

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

-- if any codepoint is used to represent a substring, the total effect
-- must be one of size reduction.  If no compression was done, the
-- size is unchanged.
propertyCompressionSavesSpaceOrId :: InputString -> Bool
propertyCompressionSavesSpaceOrId (IS s) = maybe False savesSpace $ compressText s
  where savesSpace (cpAssoc, compressed) =
            null cpAssoc && B.length compressed == length s
         ||   (sum $ map ((1+) . B.length . snd) cpAssoc) + B.length compressed
            < length s

propertyAllCompressionsUsed :: InputString -> Bool
propertyAllCompressionsUsed = maybe False allUsed . compressText . unIS
  where allUsed (cpAssoc, compressed) = all (inCompressedOrSubstrings compressed cpAssoc) cpAssoc
        inCompressedOrSubstrings compressed cpAssoc (cp, _) =
          B.elem cp compressed || (any (B.elem cp . snd)
                                     $ filter ((/= cp) . fst) cpAssoc)

-- Wrap Digram to avoid `orphaned instance' warning:
newtype ArbDigram = ArbDigram Digram deriving (Show, Eq)

instance Arbitrary ArbDigram where
  arbitrary = do
                a <- arbitrary
                b <- arbitrary
                return $ ArbDigram $ shiftDigram a $ shiftDigram b initDigram

-- There are two implementations for the `overlaps' predicate: a
-- generic one working on any candidate and a specialised one, only
-- working on a pair of Digrams.  Check that both implementations are
-- equivalent.
propertyOverlapEquivalence :: ArbDigram -> ArbDigram -> Bool
propertyOverlapEquivalence (ArbDigram x) (ArbDigram y) =
       overlapsDigram x y
    == (overlaps `on` (flip Candidate 1 . unDigram)) x y

newtype CandList = CandList [(ArbDigram, Count)] deriving (Show, Eq)

instance Arbitrary CandList where
  -- Produce a valid list of Digram candidates (i.e. where they appear
  -- often enough) by forcing each Digram to be unique and all counts
  -- to be at least minCountLen2 and sorting from most occuring to
  -- least occuring.
  arbitrary = do
      digrams <- fmap nub $ arbitrary
      counts <- arbitrary
      return . CandList $ zip digrams
                            $ reverse . sort $ map ((+minCountLen2) . abs) counts

propertyGetNextCandidatesEquivalence :: CandList -> Bool
propertyGetNextCandidatesEquivalence (CandList xs) =
       (getNextCandidates $ map arbToCand xs)
    == (map digramToCand $ getNextDigrams $ map toDigram xs)
  where arbToCand (ArbDigram d, c) = Candidate (unDigram d) c
        digramToCand (d, c) = Candidate (unDigram d) c
        toDigram (ArbDigram d, c) = (d, c)

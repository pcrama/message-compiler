module Main

where

import Data.Monoid ()
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import qualified TestDigram
import qualified TestEnnGramMap
import qualified TestCandSel
import qualified Properties

main :: IO ()
main = defaultMainWithOpts
       [ testCase "unittests.Digram" testDigram
       , testCase "unittests.EnnGramList" testMakeEnnGramList
       , testCase "unittests.EnnGramMap" testMakeEnnGramMap
       , testCase "unittests.CandSelMkCnd" testMakeCandidates
       , testCase "unittests.CandSelDropSafeOverlap" testDropSafeOverlap
       , testCase "unittests.CandSelOverlaps" testOverlaps
       , testCase "unittests.CandSelGetNextCnd" testGetNextCandidates
       , testCase "unittests.CandSelMakeThenGet"
                $ assertRightTrue TestCandSel.testMakeThenGetCandidates
       , testProperty "prop.compressDecompressId" propCompressDecompressId
       , testProperty "prop.compressionSavesSpaceOrId" propCompressSavesSpaceOrId
       , testProperty "prop.allCompressionsUsed" propAllCompressionsUsed
       ] mempty

assertRightTrue :: Show a => Either a Bool -> Assertion
assertRightTrue z@(Left _) = assertFailure $ "Expected Right True, got " ++ show z
assertRightTrue (Right z) = assertBool (show z ++ "is not True") z

testDigram :: Assertion
testDigram = assertRightTrue TestDigram.testDigramTable

testMakeEnnGramMap :: Assertion
testMakeEnnGramMap = assertRightTrue TestEnnGramMap.testMakeEnnGramMap

testMakeEnnGramList :: Assertion
testMakeEnnGramList = assertRightTrue TestEnnGramMap.testMakeEnnGramList

testMakeCandidates :: Assertion
testMakeCandidates = assertRightTrue TestCandSel.testMakeCandidates

testDropSafeOverlap :: Assertion
testDropSafeOverlap = assertRightTrue TestCandSel.testDropSafeOverlap

testOverlaps :: Assertion
testOverlaps = assertRightTrue TestCandSel.testOverlaps

testGetNextCandidates :: Assertion
testGetNextCandidates = assertRightTrue TestCandSel.testGetNextCandidates

propCompressDecompressId :: Property
propCompressDecompressId = property $ Properties.propertyCompressionIsReversible

propCompressSavesSpaceOrId :: Property
propCompressSavesSpaceOrId = property $ Properties.propertyCompressionSavesSpaceOrId

propAllCompressionsUsed :: Property
propAllCompressionsUsed = property $ Properties.propertyAllCompressionsUsed

module Main

where

import Data.Monoid ()
import Test.Framework
import Test.Framework.Providers.HUnit
-- import Test.Framework.Providers.QuickCheck2
import Test.HUnit
-- import Test.QuickCheck

import qualified TestDigram
import qualified TestEnnGramMap

main :: IO ()
main = defaultMainWithOpts
       [ testCase "unittests.Digram" testDigram
       , testCase "unittests.EnnGramList" testMakeEnnGramList
       , testCase "unittests.EnnGramMap" testMakeEnnGramMap
       -- , testProperty "listRevRevId" propListRevRevId
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

-- propListRevRevId :: [Int] -> Property
-- propListRevRevId xs = not (null xs) ==> reverse (reverse xs) == xs

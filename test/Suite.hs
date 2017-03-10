module Main

where

import Data.Monoid ()
import Test.Framework
import Test.Framework.Providers.HUnit
-- import Test.Framework.Providers.QuickCheck2
import Test.HUnit
-- import Test.QuickCheck

import TestDigram

main :: IO ()
main = defaultMainWithOpts
       [ testCase "unittests" testUnitTests
       -- , testProperty "listRevRevId" propListRevRevId
       ] mempty

testUnitTests :: Assertion
testUnitTests = TestDigram.testDigramTable @?= Right True

-- propListRevRevId :: [Int] -> Property
-- propListRevRevId xs = not (null xs) ==> reverse (reverse xs) == xs

module TestNaive
    (runTests) where

import qualified UnitTests as UT
import qualified Naive as N

substringOccurenceEq :: N.SubstringOccurences -> UT.FakeSubstringOccurences -> Bool
substringOccurenceEq act exp =
    (act_str == exp_str) && (act_cnt == exp_cnt)
    where act_str = N.substring act
          act_cnt = N.soCount act
          exp_str = UT.substring exp
          exp_cnt = UT.count exp

runTests :: Either String Bool
runTests = UT.test_getSubstrings
    (filter (\x -> N.substringLen x >= 2) . N.getSubstrings)
    substringOccurenceEq

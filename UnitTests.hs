module UnitTests
    (test_getSubstrings
    ) where

import Data.List (find
                 ,foldl')

test_getSubstrings :: ([String] -> [a]) -> (a -> a -> Bool) -> (Either String Bool)
test_getSubstrings getSubstringOccurences substringOccurenceEquality =
    compareSubstringResults [] getSubstringOccurences [] substringOccurenceEquality

exec_test_getSubstrings :: [String] -> ([String] -> [a]) -> [a] -> (a -> a -> Bool) -> (Either String Bool)
exec_test_getSubstrings input getSubstringOccurences expected substringOccurenceEquality = case isOK of
    True -> Right True
    Left -> "Unexpected output " ++ (show occurences) ++ " for input " ++ (show input)
    where isOK = compareSubstringResults occurences substringOccurenceEquality expected
          pccurences = getSubstringOccurences input


compareSubstringResults :: [a] -> (a -> a -> Bool) -> [a] -> Bool
compareSubstringResults actual eq_test expected =
    foldl' (\ accum act -> accum && isElement act expected) True actual
    where isElement act exp = case (find (eq_test act) exp) of
                                Just x -> True
                                Nothing -> False

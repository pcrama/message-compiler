module UnitTests
    (test_getSubstrings
    ,FakeSubstringOccurences
    ,substring
    ,count) where

import Data.List (find
                 ,foldl')

data FakeSubstringOccurences = FakeSubstringOccurences {
        substring :: String
      , count :: Int
} deriving (Show, Eq)

test_getSubstrings :: (Show a) => ([String] -> [a]) -> (a -> FakeSubstringOccurences -> Bool) -> (Either String Bool)
test_getSubstrings getSubstringOccurences substringOccurenceEquality =
    foldl' (\ accum (input, expected) -> case accum of
                Left x -> accum
                Right True -> exec_test_getSubstrings input
                                                      getSubstringOccurences
                                                      expected
                                                      substringOccurenceEquality)
           (Right True)
           (map (\ (inp, sub_cnt) -> (
                    inp, map (\ (sub, cnt) ->
                        FakeSubstringOccurences sub cnt)
                             sub_cnt))
                [([], [])
                ,-- only unique substrings
                (["abcde"], [("ab", 1),
                             ("abc", 1),
                             ("abcd", 1),
                             ("abcde", 1),
                             ("bc", 1),
                             ("bcd", 1),
                             ("bcde", 1),
                             ("cd", 1),
                             ("cde", 1),
                             ("de", 1)])
                ,-- only unique substrings, shared across 2 strings
                (["abcde", "bcdef"], [
                             ("ab", 1),
                             ("abc", 1),
                             ("abcd", 1),
                             ("abcde", 1),
                             ("bc", 2),
                             ("bcd", 2),
                             ("bcde", 2),
                             ("bcdef", 1),
                             ("cd", 2),
                             ("cde", 2),
                             ("cdef", 1),
                             ("de", 2),
                             ("def", 1),
                             ("ef", 1)])
                ,-- overlapping occurences of substrings don't count
                (["aaaaaa", "ababab"], [
                    ("aa", 3),
                    ("aaa", 2),
                    ("aaaa", 1),
                    ("aaaaa", 1),
                    ("aaaaaa", 1),
                    ("ab", 3),
                    ("aba", 1),
                    ("abab", 1),
                    ("ababa", 1),
                    ("ababab", 1),
                    ("ba", 2),
                    ("bab", 1),
                    ("baba", 1),
                    ("babab", 1)])])

exec_test_getSubstrings :: (Show a) => [String] -> ([String] -> [a]) -> [FakeSubstringOccurences] -> (a -> FakeSubstringOccurences -> Bool) -> (Either String Bool)
exec_test_getSubstrings input getSubstringOccurences expected substringOccurenceEquality = case isOK of
    True -> Right True
    False -> Left $ "Unexpected output " ++ (show occurences) ++ " for input " ++ (show input)
    where isOK = compareSubstringResults occurences substringOccurenceEquality expected
          occurences = getSubstringOccurences input

compareSubstringResults :: [a] -> (a -> FakeSubstringOccurences -> Bool) -> [FakeSubstringOccurences] -> Bool
compareSubstringResults actual eq_test expected =
    (length actual == length expected) &&
    foldl' (\ accum act -> accum && isElement act expected) True actual
    where isElement act exp = case (find (eq_test act) exp) of
                                Just x -> True
                                Nothing -> False

module TestCandSel

where

import Utils
import InputText
import Digram
import EnnGram
import CandidateSelection

mkCand :: (String, Count) -> Candidate
mkCand (s, c) = Candidate s (length s) c

oneTestMakeCandidates :: [String] -> [(String, Count)]
                      -> Either ([Candidate], [(String, Count)])
                                Bool
oneTestMakeCandidates sl exp =
  maybe (Right False) id $ do
    it <- toCodepoints sl
    let cands = uncurry makeCandidates . ennGramMap $ it
    if length cands == length exp
       && and (zipWith (==) cands $ map mkCand exp)
    then return $ Right True
    else return $ Left (cands, exp)

testMakeCandidates = do
  oneTestMakeCandidates ["abc", "abc"] []
  oneTestMakeCandidates ["abcde", "abcde"]
                        [("abcde", 2)
                        , ("bcde", 2), ("abcd", 2)]
  oneTestMakeCandidates ["abcde", "abcde", "bcd"]
                        [("abcde", 2)
                        , ("bcd", 3)
                        , ("bcde", 2), ("abcd", 2)]
  oneTestMakeCandidates ["abcde", "abcde", "bcd", "abc"]
                        [("abcde", 2)
                        , ("bcd", 3), ("abc", 3)
                        , ("bcde", 2), ("abcd", 2)
                        , ("bc", 4)]
  oneTestMakeCandidates ["abcde", "abcde", "bcd", "abc"]
                        [("abcde", 2)
                        , ("bcd", 3), ("abc", 3)
                        , ("bcde", 2), ("abcd", 2)
                        , ("bc", 4)]

testDropSafeOverlap :: Either ((String, Count), (String, Count))
                              Bool
testDropSafeOverlap = do
    t "abcde" 4 "abcd" 4 True
    t "abcde" 4 "bcde" 4 True
    t "abcde" 4 "abc" 4 True
    t "abcde" 4 "abd" 4 False
    t "abcde" 4 "abz" 4 False
    t "abcd" 4 "abc" 4 True
    t "abcd" 4 "abc" 5 False
    t "abcd" 4 "bcd" 4 True
    t "abcd" 4 "bc" 4 True
    t "abcd" 4 "abcde" 4 False
    t "abcd" 4 "defg" 4 False
    t "abcd" 4 "xyz" 4 False
  where t :: String -> Count -> String -> Count -> Bool
          -> Either ((String, Count), (String, Count))
                    Bool
        t a c x y e =
          if e == dropSafeOverlap (mkCand (a, c))
                                  (mkCand (x, y))
          then Right True
          else Left ((a, c), (x, y))

testOverlaps = do
    t "abcd" "abc" True
    t "abcd" "ca" True
    t "abcd" "cab" True
    t "abcd" "bac" False
    t "abcd" "bacd" False
    t "abcd" "dacb" True
    t "abcd" "dbc" True
    t "abcd" "dbca" True
    t "abcd" "bcd" True
    t "abcd" "bc" True
    t "abcd" "abcde" True
    t "abcd" "defg" True
    t "abcd" "xyz" False
    t "abcd" "xyzd" False
    t "abcd" "axyz" False
    t "abcd" "xyza" True
    t "abcd" "dxyz" True
    t "abcd" "dxyza" True
    t "abcd" "xyabcdz" True
  where t :: String -> String -> Bool
          -> Either (String, String) Bool
        t a x e =
          if e == overlaps (mkCand (a, undefined))
                           (mkCand (x, undefined))
          && e == overlaps (mkCand (x, undefined))
                           (mkCand (a, undefined))
          then Right True
          else Left (a, x)
  
oneTestGetNextCandidates a exp =
    if obs == map mkCand exp
    then Right True
    else Left (obs, exp)
  where obs = getNextCandidates $ map mkCand a

testGetNextCandidates = do
  oneTestGetNextCandidates [("abcde", 4)
                           , ("abcd", 4)
                           , ("bcde", 4)
                           , ("abc", 4)
                           , ("abd", 4)
                           , ("abz", 4)
                           , ("bcd", 4)
                           , ("cde", 4)
                           , ("def", 4)
                           , ("xy", 4)]
                           [("abcde", 4)
                           , ("abd", 4)
                           , ("abz", 4)]
  oneTestGetNextCandidates [("abcde", 4)]
                           [("abcde", 4)]
  oneTestGetNextCandidates [("abcde", 4)
                           , ("xabc", 5)
                           , ("yabc", 5)
                           , ("zabc", 5)
                           , ("bcde", 4)
                           , ("abc", 4)
                           , ("abd", 4)
                           , ("abz", 4)
                           , ("bcd", 4)
                           , ("cde", 4)
                           , ("def", 4)
                           , ("xy", 4)]
                           [("abcde", 4)]
  oneTestGetNextCandidates [("abcde", 4)
                           , ("cdex", 5)
                           , ("bcde", 4)
                           , ("abc", 4)
                           , ("abd", 4)
                           , ("abz", 4)
                           , ("bcd", 4)
                           , ("cde", 4)
                           , ("def", 4)
                           , ("xy", 4)]
                           [("abcde", 4)]

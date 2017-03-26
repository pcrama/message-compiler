module TestCandSel (
    testMakeCandidates
  , testDropSafeOverlap
  , testOverlaps
  , testGetNextCandidates
  , testMakeThenGetCandidates
)

where

-- import Debug.Trace (trace)
import Data.Char (ord)
import Data.List (sort)
import qualified Data.ByteString as B

import Utils
import InputText
import EnnGram
import CandidateSelection

-- traceId :: Show x => x -> x
-- traceId x = trace (show x) x

mkCand :: (String, Count) -> Candidate
mkCand (s, c) = Candidate (B.pack . map (fromIntegral . ord) $ s) c

oneTestMakeCandidates :: [String] -> [(String, Count)]
                      -> Either ([Candidate], [(String, Count)])
                                Bool
oneTestMakeCandidates sl xpct =
  maybe (Right False) id $ do
    it <- toCodepoints sl
    let cands = uncurry (makeCandidates maxCompressions) . ennGramMap $ it
    if length cands == length xpct
       && and (zipWith (==) cands $ map mkCand xpct)
    then return $ Right True
    else return $ Left (cands, xpct)

testMakeCandidates :: Either ([Candidate], [(String, Count)]) Bool
testMakeCandidates =
     oneTestMakeCandidates ["abc", "abc"] []
  >> oneTestMakeCandidates ["abcde", "abcde"]
                           [("abcde", 2)
                           , ("bcde", 2), ("abcd", 2)]
  >> oneTestMakeCandidates ["abcde", "abcde", "bcd"]
                           [("abcde", 2)
                           , ("bcd", 3)
                           , ("bcde", 2), ("abcd", 2)]
  >> oneTestMakeCandidates ["abcde", "abcde", "bcd", "abc"]
                           [("abcde", 2)
                           , ("bcd", 3), ("abc", 3)
                           , ("bcde", 2), ("abcd", 2)
                           , ("bc", 4)]
  >> oneTestMakeCandidates ["abcde", "abcde", "bcd", "abc"]
                           [("abcde", 2)
                           , ("bcd", 3), ("abc", 3)
                           , ("bcde", 2), ("abcd", 2)
                           , ("bc", 4)]
  >> oneTestMakeCandidates ["aab", "aab", "aac", "abd", "aae", "abf", "aa"]
                           -- but not ("ab", 4): makeCandidates takes
                           -- the minimum number of Digram candidates
                           -- and as a side-effect, they are already
                           -- filtered not to overlap.
                           [("aa", 5)]

testDropSafeOverlap :: Either ((String, Count), (String, Count))
                              Bool
testDropSafeOverlap =
       t "abcde" 4 "abcd" 4 True
   >>  t "abcde" 4 "bcde" 4 True
   >>  t "abcde" 4 "abc" 4 True
   >>  t "abcde" 4 "abd" 4 False
   >>  t "abcde" 4 "abz" 4 False
   >>  t "abcd" 4 "abc" 4 True
   >>  t "abcd" 4 "abc" 5 False
   >>  t "abcd" 4 "bcd" 4 True
   >>  t "abcd" 4 "bc" 4 True
   >>  t "abcd" 4 "abcde" 4 False
   >>  t "abcd" 4 "defg" 4 False
   >>  t "abcd" 4 "xyz" 4 False
  where t :: String -> Count -> String -> Count -> Bool
          -> Either ((String, Count), (String, Count))
                    Bool
        t a c x y e =
          if e == dropSafeOverlap (mkCand (a, c))
                                  (mkCand (x, y))
          then Right True
          else Left ((a, c), (x, y))

testOverlaps :: Either (String, String) Bool
testOverlaps =
       t "abcd" "abc" True
   >>  t "abcd" "ca" True
   >>  t "abcd" "cab" True
   >>  t "abcd" "bac" False
   >>  t "abcd" "bacd" False
   >>  t "abcd" "dacb" True
   >>  t "abcd" "dbc" True
   >>  t "abcd" "dbca" True
   >>  t "abcd" "bcd" True
   >>  t "abcd" "bc" True
   >>  t "abcd" "abcde" True
   >>  t "abcd" "defg" True
   >>  t "abcd" "xyz" False
   >>  t "abcd" "xyzd" False
   >>  t "abcd" "axyz" False
   >>  t "abcd" "xyza" True
   >>  t "abcd" "dxyz" True
   >>  t "abcd" "dxyza" True
   >>  t "abcd" "xyabcdz" True
  where t :: String -> String -> Bool
          -> Either (String, String) Bool
        t a x e =
          if e == overlaps (mkCand (a, undefined))
                           (mkCand (x, undefined))
          && e == overlaps (mkCand (x, undefined))
                           (mkCand (a, undefined))
          then Right True
          else Left (a, x)
  
oneTestGetNextCandidates :: [(String, Count)]
                            -> [(String, Count)]
                            -> Either ([Candidate], [(String, Count)]) Bool
oneTestGetNextCandidates a xpct =
    if obs == map mkCand xpct
    then Right True
    else Left (obs, xpct)
  where obs = getNextCandidates $ map mkCand a

testGetNextCandidates :: Either ([Candidate], [(String, Count)]) Bool
testGetNextCandidates =
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
  >> oneTestGetNextCandidates [("abcde", 4)]
                              [("abcde", 4)]
  >> oneTestGetNextCandidates [("abcde", 4)
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
  >> oneTestGetNextCandidates [("abcde", 4)
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
  -- see also oneTestMakeThenGetCandidates that integrates these 3 cases
  >> oneTestGetNextCandidates [("aa", 6), ("ab", 5)]
                              [("aa", 6)]
  >> oneTestGetNextCandidates [("aa", 6), ("ab", 5), ("ff", 5)]
                              [("aa", 6), ("ff", 5)]
  >> oneTestGetNextCandidates [("!!", 16),("!!?", 5),("!?", 11),("**", 11),("++", 11),("--", 11),("//", 11),("00", 11),("11", 11),("22", 11),("33", 11),("44", 11),("55", 11),("66", 11),("77", 11),("88", 11),("99", 11),("AA", 11),("BB", 11),("CC", 11),("DD", 11),("EE", 11),("FF", 11),("GG", 11),("HH", 11),("II", 11),("JJ", 11),("KK", 11),("LL", 11),("MM", 11),("NN", 11),("OO", 11),("PP", 11),("QQ", 11),("RR", 11),("SS", 11),("TT", 11),("UU", 11),("VV", 11),("WW", 11),("XX", 11),("YY", 11),("ZZ", 11),("__", 11),("aa", 11),("bb", 11),("cc", 11),("dd", 11),("ee", 11),("ff", 11),("gg", 11),("hh", 11),("ii", 11),("jj", 11),("kk", 11),("ll", 11),("mm", 11),("nn", 11),("oo", 11),("pp", 11),("qq", 11),("rr", 11),("ss", 11),("tt", 11),("uu", 11)]
                              [("!!", 16),("**", 11),("++", 11),("--", 11),("//", 11),("00", 11),("11", 11),("22", 11),("33", 11),("44", 11),("55", 11),("66", 11),("77", 11),("88", 11),("99", 11),("AA", 11),("BB", 11),("CC", 11),("DD", 11),("EE", 11),("FF", 11),("GG", 11),("HH", 11),("II", 11),("JJ", 11),("KK", 11),("LL", 11),("MM", 11),("NN", 11),("OO", 11),("PP", 11),("QQ", 11),("RR", 11),("SS", 11),("TT", 11),("UU", 11),("VV", 11),("WW", 11),("XX", 11),("YY", 11),("ZZ", 11),("__", 11),("aa", 11),("bb", 11),("cc", 11),("dd", 11),("ee", 11),("ff", 11),("gg", 11),("hh", 11),("ii", 11),("jj", 11),("kk", 11),("ll", 11),("mm", 11),("nn", 11),("oo", 11),("pp", 11),("qq", 11),("rr", 11),("ss", 11),("tt", 11),("uu", 11)]

oneTestMakeThenGetCandidates :: [String]
                             -> [(String, Count)]
                             -> Either ([String], [Candidate]) Bool
oneTestMakeThenGetCandidates input xp =
    if observed == map mkCand xp then Right True else Left (input, observed)
  where observed = getNextCandidates
                 $ uncurry (makeCandidates maxCompressions)
                 $ ennGramMap
                 $ maybe undefined id
                 $ toCodepoints input

testMakeThenGetCandidates :: Either ([String], [Candidate]) Bool
testMakeThenGetCandidates =
     oneTestMakeThenGetCandidates ["aab", "aab", "aac", "aad", "aae",
                                   "aa", "ab", "ab", "ab",
                                   "ff", "ff", "ff", "ff"]
                                  -- *NOT* ("ab", 5) because it is
                                  -- invalidated by ("aa", 6)
                                  -- *NOT* ("ff", 4) because after we
                                  -- dropped the better ("ab", 5), we
                                  -- need to recount if ("ff", 4)
                                  -- would beat the new "ab" count
                                  -- after replacing all "aa". In this
                                  -- case, it would, but see the next
                                  -- test.
                                  [("aa", 6)]
  >> oneTestMakeThenGetCandidates ["aa", "aa", "aac", "aad", "aae",
                                   "aa", "ab", "ab", "ab", "ab", "ab",
                                   "ff", "ff", "ff", "ff"]
                                  -- *NOT* ("ab", 5) because it is
                                  -- invalidated by ("aa", 6)
                                  -- *NOT* ("ff", 4) because after we
                                  -- dropped the better ("ab", 5), we
                                  -- need to recount if ("ff", 4)
                                  -- would beat the new "ab" count
                                  -- after replacing all "aa".  In
                                  -- this case, it wouldn't, but see
                                  -- the previous test.
                                  [("aa", 6)]
  >> oneTestMakeThenGetCandidates ["aab", "aab", "aac", "aad", "aae",
                                   "aa", "ab", "ab", "ab",
                                   "ff", "ff", "ff", "ff", "ff"]
                                  -- *NOT* ("ab", 5) because it is
                                  -- invalidated by ("aa", 6)
                                  -- ("ff", 5) is independent of
                                  -- ("ab", 5) and ("aa", 6) and just
                                  -- as good as ("ab", 5) so we can
                                  -- keep it
                                  [("aa", 6), ("ff", 5)]
  -- The idea of this test case is to check that when there are more
  -- candidates than the number of candidates requested, the exact
  -- number is returned (even if because of overlap, some candidates
  -- were dropped along the way)
  --
  -- preconditions for test case are verified below
  >> let alphabet = sort "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789+-*/_()"
         intToString x = replicate 2 $ alphabet !! x
         -- Utils.hs: compressionGain = (count - 1) * (fromIntegral len - 1) - 2
         n = 11 -- 10 * 1 - 2 = 8
         m = 6  --  5 * 2 - 2 = 8
         spec1 = '!'
         spec2 = '?'
         t1 = [intToString x | x <- [0..maxCompressions + 2]]
         t2 = [spec1, spec1]:t1 -- prepended literal should have nothing in common with `alphabet'
         t3 = concat $ (replicate m [spec1, spec1, spec2]) -- "!!?"
                      :(replicate n $ spec1:"a")           -- "!a"
                      :(replicate n $ spec1:"b")           -- "!b"
                      :(replicate n $ spec1:"c")           -- "!c"
                      :replicate n t2
     in either (Left . id)
               -- verify preconditions for test case:
               (Right . (&& compressionGain m 3 == compressionGain n 2
                         && compressionGain n 2 > 0
                         && length alphabet > maxCompressions
                         && not (spec1 `elem` alphabet)
                         && not (spec2 `elem` alphabet)
                         && spec1 /= spec2))
             $ oneTestMakeThenGetCandidates t3
                                            $ ("!!", n + m):[(intToString x, n)
                                                             | x <- [0..maxCompressions - 2]]

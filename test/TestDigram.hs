module TestDigram (
  testDigram
, testDigramTable
, oneTest
) where

import Data.Array.IArray
import Data.Traversable (traverse)
import qualified Data.ByteString as B

import InputText
import Digram
import Utils

testDigramTable :: Either (Maybe [(B.ByteString, Int)]) Bool
testDigramTable =
     testDigram [""] []
  >> testDigram ["abcd"] [("ab",1),("bc",1),("cd",1)]
  >> testDigram ["abcd","abcd"] [("ab",2),("bc",2),("cd",2)]
  >> testDigram ["ab","cd"] [("ab",1),("cd",1)]
  >> testDigram ["aaaa"] [("aa",2)]
  >> testDigram ["bbbb","bbbbbbbb"] [("bb",6)]
  >> return True

testDigram :: [String] -> [(String, Int)]
           -> Either (Maybe [(B.ByteString, Utils.Count)])
                     [(B.ByteString, Utils.Count)]
testDigram ss exp_dt = case oneTest ss $ fakeDigramTable exp_dt of
  Left Nothing -> Left Nothing
  Left (Just x) -> Left . Just $ summarizeDigramTable x
  Right x -> Right $ summarizeDigramTable x

fakeDigramTable :: [(String, Int)] -> DigramTable
fakeDigramTable =
    accumArray (flip const) 0 (minBound, maxBound)
  . map xform
  where xform (s, count) = (string2digram s, count)
        string2digram s =
          case traverse charToCodepoint s of
            Just (a:b:_) -> shiftDigram b $ shiftDigram a initDigram
            Just [a] -> shiftDigram a initDigram
            Just [] -> initDigram
            Nothing -> initDigram

oneTest :: [String] -> DigramTable -> Either (Maybe DigramTable) DigramTable
oneTest ss exp_dt = case toCodepoints ss of
  Just it -> let obs_dt = digramTable it
             in (if exp_dt == obs_dt
                 then Right
                 else Left . Just) obs_dt
  Nothing -> Left Nothing

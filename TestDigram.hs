module TestDigram (
  testDigram
, oneTest
) where

import Data.Array
import Data.Word (Word16)
import Data.Traversable (traverse)

import InputText
import Digram

instance Monad (Either a) where
  return = Right
  (Left x) >>= _k = Left x
  (Right x) >>= k = k x

testDigramTable :: Either (Maybe [(String, Int)]) Bool
testDigramTable = do
  testDigram [""] []
  testDigram ["abcd"] [("ab",1),("bc",1),("cd",1)]
  testDigram ["abcd","abcd"] [("ab",2),("bc",2),("cd",2)]
  testDigram ["ab","cd"] [("ab",1),("cd",1)]
  testDigram ["aaaa"] [("aa",2)]
  testDigram ["bbbb","bbbbbbbb"] [("bb",6)]
  return True

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
  
foldArray :: (Num a, Ix a) => (b -> c -> b) -> b -> Array a c -> b
foldArray f base arr = go min base
  where (min, max) = bounds arr
        go idx acc | idx == max = acc
                   | otherwise = let fax = f acc (arr ! idx) in
                                  fax `seq` go (idx + 1) fax

sumTo :: Word16 -> Word16
sumTo x = go 0 0
  where go idx acc | idx > x = acc
                   | otherwise = let su = idx + acc in
                       su `seq` go (idx + 1) su

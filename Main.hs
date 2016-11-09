module Main

where

import Data.Ix (Ix, range)
import Data.Array ((!), Array, accumArray, array, bounds)
import Data.Char (ord)

import Digram
import EnnGram
import InputText
import Reader

Just txt = toCodepoints ["HellWo", "World", "Wouf"]
Just ng = mkEnnGram (2 :: Int) (3 :: Int)

digramTable :: InputText -> Array Digram Int
digramTable txt =
    accumArray (+) 0 (minBound, maxBound) . map (flip (,) 1) $
      digramList
  where digramList = filter (not . stringBoundary) $
                            foldArray tally [Digram 0] txt
        tally :: Codepoint -> [Digram] -> [Digram]
        tally c z@(x:_) = (shiftDigram c x):z

foldArray :: Ix a => (b -> c -> c) -> c -> Array a b -> c
foldArray fun base arr =
    myFoldl' combine base (range $ bounds arr)
  where combine xs idx = fun (arr ! idx) xs

foldEnumArray :: Ix a => ((a, b) -> c -> c) -> c -> Array a b -> c
foldEnumArray fun base arr =
    myFoldl' combine base (range $ bounds arr)
  where combine xs idx = fun (idx, (arr ! idx)) xs

myFoldl' :: (a -> b -> a) -> a -> [b] -> a
myFoldl' _ a [] = a
myFoldl' f a (x:xs) = let fax = f a x in fax `seq` myFoldl' f fax xs

data CombineState = CS { start :: Int, digram :: Digram, tail :: [InputText] }

combineRestart :: Int -> Codepoint -> CombineState -> CombineState
combineRestart idx ch cs = cs { start = idx
                              , digram = shiftDigram ch initDigram }

combine :: InputText -> Array Digram Int 
           -> (Int, Codepoint) -> CombineState -> CombineState
combine txt digTable (idx, ch) cs
    | maxOccur < 2) and (idx > st + 
        if maxOccur > 1
        then cs { digram = newDi
                , tail = foldr (\len t ->
                                   (runReader
                                    (ennString $ mkEnnGram st len)
                                    txt):
                                   t)
                               (tail cs)
                               [3..(idx - st + 1)] }
    | otherwise = cs { digram = newDi }
  where st = start cs
        di = digram cs
        newDi = shiftDigram ch di 
        maxOccur = digTable ! newDi

module InputText (
  Codepoint
, InputText
, toCodepoints
, charToCodepoint
)

where

import qualified Data.Array as A
import qualified Data.ByteString as B
import Data.Traversable (traverse)
import Data.Char (ord)
import Data.Word (Word8)
import Debug.Trace (trace)
import Data.List (intercalate)

import Utils

type Codepoint = Word8

type InputText = B.ByteString

toCodepoints :: [String] -> Maybe InputText
toCodepoints = fmap (B.pack . intercalate [fromIntegral stringSeparationCP])
             . traverse (traverse charToCodepoint)

validCodepoint :: Char -> Bool
validCodepoint x = (x >= '\x20') && (x < '\xa0')

charToCodepoint :: Char -> Maybe Codepoint
charToCodepoint x | validCodepoint x = Just . fromIntegral $ ord x
                  | otherwise = Nothing

stringToCodepoint :: String -> Maybe [Codepoint]
stringToCodepoint xs =
  fmap (++[0]) $ traverse charToCodepoint xs

module InputText (
  Codepoint
, InputText
, Offset
, toCodepoints
, charToCodepoint
)

where

import qualified Array as A
import Data.Traversable (traverse)
import Data.Char (ord)
import Data.Word (Word8)
import Debug.Trace (trace)

type Codepoint = Word8
type Offset = Int

type InputText = A.Array Offset Codepoint

toCodepoints :: [String] -> Maybe InputText
toCodepoints xs = do
  asCodepoints <- traverse stringToCodepoint xs
  let all = concat asCodepoints
  return $ A.array (0, length all - 1)
                   (zipWith (,) [0..] all)

validCodepoint :: Char -> Bool
validCodepoint x = (x >= '\x20') && (x < '\xa0')

charToCodepoint :: Char -> Maybe Codepoint
charToCodepoint x | validCodepoint x = Just . fromIntegral $ ord x
                  | otherwise = Nothing

stringToCodepoint :: String -> Maybe [Codepoint]
stringToCodepoint xs =
  fmap (++[0]) $ traverse charToCodepoint xs

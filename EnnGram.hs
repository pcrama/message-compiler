module EnnGram (
  EnnGram
, ennLength
, ennOffs
, ennString
, mkEnnGram
) where

import qualified Array as A
import Data.Word (Word32)
import Data.Bits ((.&.), shift, Bits)
import Data.Map (fromListWith)
import Data.Char (chr)

import InputText
import Reader

newtype EnnGram = EnnGram Word32

instance Show EnnGram where
  show x = "EG " ++ (show $ ennOffs x) ++ ":" ++ (show $ ennLength x)

--mkEnnGram :: (Integral len, Bits len) => Offset -> len -> Maybe EnnGram
mkEnnGram :: Offset -> Length -> Maybe EnnGram
mkEnnGram offs len =
  if (offs < maxOffs) && (len > 2) && (len < maxLen)
  then Just . EnnGram $ (fromIntegral offs `shift` lenWidth)
                        + fromIntegral len
  else Nothing
  where maxOffs = 1 `shift` 20
        maxLen = 1 `shift` lenWidth
        lenWidth = 12 -- bits

ennString :: EnnGram -> Reader InputText String
ennString x = do
  txt <- ask
  let len = fromIntegral $ ennLength x
  let offs = fromIntegral $ ennOffs x
  return $ foldr (\idx t -> (chr' $ txt A.! idx):t) [] [offs..(offs + len - 1)]
 where chr' = chr . fromInteger . toInteger

ennOffs :: EnnGram -> Offset
ennOffs (EnnGram x) = fromIntegral $ x `shift` (0 - 12)

ennLength :: EnnGram -> Length
ennLength (EnnGram x) = fromIntegral $ x .&. 0xfff

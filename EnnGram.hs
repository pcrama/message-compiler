module EnnGram (
  EnnGram
, ennLength
, ennString
, mkEnnGram
) where

import qualified Array as A
import Data.Word (Word32)
import Data.Bits ((.&.), shift, Bits)
import Data.Map (fromListWith)

import InputText
import Reader

newtype EnnGram = EnnGram Word32

-- mkEnnGram :: (Integral a, Integral b) =>
--              a -> b -> Maybe EnnGram
mkEnnGram offs len =
  if (offs < maxOffs) && (len > 2) && (len < maxLen)
  then Just . EnnGram $ (fromIntegral offs `shift` 12)
                        + fromIntegral len
  else Nothing
  where maxOffs = 1 `shift` 20
        maxLen = 1 `shift` 12

ennString :: EnnGram -> Reader InputText InputText
ennString x = do
  txt <- ask
  let len = fromIntegral $ ennLength x
  let offs = fromIntegral $ ennOffs x
  return . A.listArray (0, len - 1) $ 
      foldr (\idx t -> ((txt A.! idx):t)) [] [offs..(offs + len - 1)]

ennOffs :: EnnGram -> Word32
ennOffs (EnnGram x) = x `shift` (0 - 12)

ennLength :: EnnGram -> Word32
ennLength (EnnGram x) = x .&. 0xfff

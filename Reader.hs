module Reader (
  ask
, asks
, Reader
, runReader
)

where

newtype Reader a b = Reader { runReader :: a -> b }

instance Functor (Reader a) where
  fmap f (Reader g) = Reader $ f . g

instance Monad (Reader a) where
  return = Reader . const
  (Reader a) >>= k = Reader $ \x ->
    let (Reader f) = k . a $ x in f x

ask :: Reader a a
ask = Reader id

asks :: (a -> b) -> Reader a b
asks f = fmap f ask

local :: (a -> b) -> Reader b c -> Reader a c
local f (Reader r) = Reader $ r . f

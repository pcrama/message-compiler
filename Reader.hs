module Reader (
  Applicative
, pure
, ap
, ask
, asks
, Reader
, runReader
)

where

newtype Reader a b = Reader { runReader :: a -> b }

instance Functor (Reader a) where
  fmap f (Reader g) = Reader $ f . g

class (Functor f) => Applicative f where
  pure :: a -> f a
  ap :: f (a -> b) -> f a -> f b

instance Applicative (Reader a) where
  pure = Reader . const
  -- Reader a b->c -> Reader a b -> Reader a c
  (Reader af) `ap` (Reader av) = Reader $ \a -> (af a) (av a)

instance Monad (Reader a) where
  return = pure
  (Reader a) >>= k = Reader $ \x ->
    let (Reader f) = k . a $ x in f x

ask :: Reader a a
ask = Reader id

asks :: (a -> b) -> Reader a b
asks f = fmap f ask

local :: (a -> b) -> Reader b c -> Reader a c
local f (Reader r) = Reader $ r . f

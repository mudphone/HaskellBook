{-# LANGUAGE InstanceSigs #-}

module TwentyFive where
import Control.Applicative (liftA2)

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) =>
         Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga


-- Twinplicative, GOTCHA! Exercise Time

instance (Applicative f, Applicative g) =>
         Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure

  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  (Compose f) <*> (Compose a) = Compose $ (liftA2 (<*>)) f a
--  (Compose f) <*> (Compose a) = Compose $ (fmap (<*>) f) <*> a


-- Exercises: Compose Instances
instance (Foldable f, Foldable g) =>
         Foldable (Compose f g) where
  foldMap f (Compose fga) = foldMap (\a -> foldMap f a) fga
  
instance (Traversable f, Traversable g) =>
         Traversable (Compose f g) where
--  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse f (Compose fga) = sequenceA (f <$> (Compose fga))


-- And now for something completely different
class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}

  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

-- 1
data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap f g = second g . first f

-- 2
data Const a b = Const a

instance Bifunctor Const where
  bimap f g = second g . first f

-- 3
data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  bimap f g = second g . first f

-- 4
data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  bimap f g = second g . first f

-- 5
data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap f g = second g . first f

-- 6
data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
  bimap f g = second g . first f 

-- 7
data Either' a b = Left a | Right b

instance Bifunctor Either' where
  bimap f g = second g . first f

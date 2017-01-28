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

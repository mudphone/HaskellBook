module Eighteen where

import Control.Applicative
import Data.Monoid
import Control.Monad (join, liftM2)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- The answer is the exercise...
bind :: Monad m => (a -> m b) -> m a -> m b
bind f a = join $ fmap f a  

-- λ> bind (\x -> [x, 1]) [1..5]
-- [1,1,2,1,3,1,4,1,5,1]

--
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else [x*x]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else []


-- Short Exercise: Either Monad
data Sum' a b =
    First' a
  | Second' b
  deriving (Eq, Show)

instance Functor (Sum' a) where
  fmap _ (First' a) = First' a
  fmap f (Second' b) = Second' (f b)

instance Applicative (Sum' a) where
  pure = Second'
  (<*>) (First' a) _ = First' a
  (<*>) _ (First' a) = First' a
  (<*>) (Second' f) (Second' b) = Second' (f b)

instance Monad (Sum' a) where
  return = pure
  (>>=) (First' a)  _ = First' a
  (>>=) (Second' a) f = f a


-- Monad Laws
-- Right identity
-- λ> Second' 5 >>= return
-- Second' 5

-- Left identity
-- λ> (return 5 :: Eighteen.Sum' Int Int) >>=  return . (+8)
-- Second' 13

-- Associativity
-- λ> (Second' '5 >>= return . (+8)) >>= return . (*2)
-- Second' 26

-- λ> Second' 5 >>= ((\x -> (return . (+8) $ x) >>= return . (*2)))
-- Second' 26


-- Chapter Exercises
-- 1
data Nope a = NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return = pure
  (>>=) _ _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where (=-=) = eq

testNope :: IO ()
testNope = do
  let trigger = undefined :: Nope (Int, String, Int)
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
  

-- 2
data PhhhbbtttEither b a =
    Left' a
  | Right' b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Left' b, Right' a]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither a b) where (=-=) = eq

instance Functor (PhhhbbtttEither b) where
  fmap _ (Right' b) = Right' b
  fmap f (Left' a) = Left' (f a)

instance Applicative (PhhhbbtttEither b) where
  pure = Left'
  (<*>) (Right' b) _ = Right' b
  (<*>) _ (Right' b) = Right' b
  (<*>) (Left' f) (Left' a) = Left' (f a)

instance Monad (PhhhbbtttEither b) where
  return = pure
  (>>=) (Right' b) _ = (Right' b)
  (>>=) (Left' a) f = f a

testPhhhbbtttEither :: IO ()
testPhhhbbtttEither = do
  let trigger = undefined :: PhhhbbtttEither String (Int, String, Int)
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger


-- 3
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where (=-=) = eq

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where
  return = pure
  (>>=) (Identity a) f = f a
  
testIdentity :: IO ()
testIdentity = do
  let trigger = undefined :: Identity (Int, String, Int)
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger


-- 4
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Monoid (List a) where
  mempty = Nil
  mappend a Nil = a
  mappend Nil a = a
  mappend (Cons x xs) ys = Cons x $ xs `mappend` ys

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a list) = Cons (f a) (fmap f list)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f b) ca = fmap f ca <> (b <*> ca)

instance Monad List where
  return = pure
  (>>=) Nil _ = Nil
  (>>=) (Cons a la) f = f a <> (la >>= f)
  
take' :: Int -> List a -> List a
take' n as = go n Nil as
  where go _ acc Nil = acc
        go 0 acc _   = acc
        go n acc (Cons x xs) = go (n - 1) (acc `append` (Cons x Nil)) xs

instance Arbitrary a => Arbitrary (List a) where
  -- this breaks
  -- arbitrary = Cons <$> arbitrary <*> arbitrary
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [(1, return Nil),
               (10, return (Cons x y))]

instance Eq a => EqProp (List a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = take' 3000 xs
          ys' = take' 3000 ys

testList :: IO ()
testList = do
  let trigger = undefined :: List (Int, String, Int)
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger


-- 1
j :: Monad m => m (m a) -> m a
j m = join m

-- 2
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

--3
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

-- 4
a4 :: Monad m => m a -> m (a -> b) -> m b
a4 ma mf = mf >>= (\f -> fmap f ma)

-- 5
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] f = return []
meh (a:as) f = (f a) >>= (\b -> fmap ([b]++) (meh as f))

-- 6
flipType :: Monad m => [m a] -> m [a]
flipType xs = meh xs id

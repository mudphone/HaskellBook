{-# LANGUAGE FlexibleContexts #-}
module TwentyOne where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 21.12 Chapter Exercises
-- Traversable instances
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Foldable Identity where
  foldMap f (Identity x) = f x
  foldr f z (Identity a) = f a z

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

instance Eq a => EqProp (Identity a) where (=-=) = eq


-- Constant
newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  traverse _ (Constant x) = pure $ Constant x

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = do
    a <- arbitrary
    return $ Constant a

instance Eq a => EqProp (Constant a b) where (=-=) = eq


-- Maybe
data Optional a =
    Nada
  | Yep a
  deriving (Eq, Ord, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep x) = Yep (f x)

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep x) = f x

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep x) = Yep <$> f x

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    elements [Nada, Yep a]

instance Eq a => EqProp (Optional a) where (=-=) = eq


-- List
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Ord, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (f <$> as)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons a as) = f a <> foldMap f as

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons a as) = Cons <$> f a <*> traverse f as 

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [(1, return Nil),
               (10, return (Cons x y))]

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

take' :: Int -> List a -> List a
take' n as = go n Nil as
  where go _ acc Nil = acc
        go 0 acc _   = acc
        go n acc (Cons x xs) = go (n - 1) (acc `append` (Cons x Nil)) xs

instance Eq a => EqProp (List a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = take' 3000 xs
          ys' = take' 3000 ys


-- Three
data Three a b c = Three a b c
  deriving (Eq, Ord, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = (Three a b) <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq


-- Three'
data Three' a b = Three' a b b
  deriving (Eq, Ord, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance Foldable (Three' a) where
  foldMap f (Three' a b c) = f b <> f c

instance Traversable (Three' a) where
  traverse f (Three' a b c) = (Three' a) <$> f b <*> f c

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Three' a b b)

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq


-- S
data S n a = S (n a) a
  deriving (Eq, Ord, Show)

instance Functor n => Functor (S n) where
  fmap f (S na a) = S (f <$> na) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S na a) = (foldMap f na) <> (f a)

instance Traversable n => Traversable (S n) where
  traverse f (S na a) = S <$> traverse f na <*> f a

instance (Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = do
    n <- arbitrary
    a <- arbitrary
    return (S n a)

instance (Eq a, Eq (n a)) => EqProp (S n a) where (=-=) = eq


-- Instances for Tree
data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Ord, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node ta a tb) = Node (f <$> ta) (f a) (f <$> tb)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node ta a tb) = (foldMap f ta) <> (f a) <> (foldMap f tb)

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node ta a tb) = Node <$> (traverse f ta) <*> (f a) <*> (traverse f tb)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    x <- arbitrary
    l <- arbitrary
    r <- arbitrary
    frequency [(1, return Empty),
               (1, return (Leaf x)),
               (1, return (Node l x r))]

instance Eq a => EqProp (Tree a) where (=-=) = eq
  
main :: IO ()
main = do
  let triggerIdentity = undefined :: Identity (Int, Int, [Int])
  quickBatch (traversable triggerIdentity)
  let triggerConstant = undefined :: Constant (Int, Int, [Int]) (Int, Int, [Int])
  quickBatch (traversable triggerConstant)
  let triggerOptional = undefined :: Optional (Int, Int, [Int])
  quickBatch (traversable triggerOptional)
  let triggerList = undefined :: List (Int, Int, [Int])
  quickBatch (traversable triggerList)
  let triggerThree = undefined :: Three Int Int (Int, Int, [Int])
  quickBatch (traversable triggerThree)
  let triggerThree' = undefined :: Three' Int (Int, Int, [Int])
  quickBatch (traversable triggerThree')
  let triggerS = undefined :: S [] (Int, String, [Int])
  quickBatch (traversable triggerS)
  let triggerTree = undefined :: Tree (Int, Int, [Int])
  quickBatch (traversable triggerTree)

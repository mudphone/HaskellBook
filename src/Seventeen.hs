module Seventeen where

import Control.Applicative
import Data.List (elemIndex)
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Exercises: Lookups
-- 1
added :: Maybe Integer
added = pure (+3) <*> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-- 2
y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- 3
x3 :: Maybe Int
x3 = elemIndex 3 [1, 2, 3, 4, 5]

y3 :: Maybe Int
y3 = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = pure max' <*> x3 <*> y3

-- 4
xs = [1, 2, 3]
ys = [4, 5, 6]

x4 :: Maybe Integer
x4 = lookup 3 $ zip xs ys

y4 :: Maybe Integer
y4 = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ (,) <$> x4 <*> y4


-- Exercise: Identity Instance
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a) 

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a) 


-- Exercise: Constant Instance
newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (<*>) (Constant a1) (Constant a2) = Constant (mappend a1 a2)


-- Exercise: Fixer Upper
-- 1
fixerUpper1 = const <$> Just "Hello" <*> pure "World"

-- 2
fixerUpper2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]


-- List Applicative Exercise
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a list) = Cons (f a) (fmap f list)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap _ Nil = Nil
flatMap f as = concat' $ fmap f as

instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) fs as = concat' $ fmap (\f -> fmap f as) fs

functions = Cons (+1) (Cons (*2) Nil)
values = Cons 1 (Cons 2 Nil)

-- λ> functions <*> values
-- Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))

toMyList = foldr Cons Nil
myXs = toMyList [1,2,3]
myC = Cons
myFlatMap = flatMap (\x -> x `myC` (9 `myC` Nil)) myXs


-- ZipList Applicative Exercise
take' :: Int -> List a -> List a
take' n as = go n Nil as
  where go _ acc Nil = acc
        go 0 acc _   = acc
        go n acc (Cons x xs) = go (n - 1) (acc `append` (Cons x Nil)) xs

newtype ZipList' a = ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a = ZipList' (Cons a Nil)
  (<*>) (ZipList' Nil) _ = (ZipList' Nil)
  (<*>) _ (ZipList' Nil) = (ZipList' Nil)
  (<*>) (ZipList' (Cons f fs)) (ZipList' (Cons x xs)) =
     ZipList' ((Cons (f x) Nil) `append` ((\(ZipList' a) -> a) ((ZipList' fs) <*> (ZipList' xs))))

z2 = ZipList' functions
z2' = ZipList' values
-- λ> z2 <*> z2'
-- ZipList' (Cons 2 (Cons 4 Nil))

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = Cons <$> arbitrary <*> arbitrary

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

testZipList :: IO ()
testZipList = quickBatch $ applicative ((ZipList' (Cons (1, "a", 'a') Nil)) :: ZipList' (Int, String, Char))
-- λ> testZipList
-- applicative:
--   identity:     *** Failed! Falsifiable (after 1 test):
-- ...
--   composition:  *** Failed! Falsifiable (after 1 test): 
-- Interrupted.


-- What about the list? => ZipList2 []
newtype ZipList2 a = ZipList2 ([a])
  deriving (Eq, Show)

instance Functor ZipList2 where
  fmap f (ZipList2 xs) = ZipList2 $ fmap f xs

instance Applicative ZipList2 where
  pure a = ZipList2 [a]
  (<*>) (ZipList2 []) _ = (ZipList2 [])
  (<*>) _ (ZipList2 []) = (ZipList2 [])
  (<*>) (ZipList2 (f:fs)) (ZipList2 (x:xs)) =
     ZipList2 ([f x] ++ ((\(ZipList2 a) -> a) ((ZipList2 fs) <*> (ZipList2 xs))))

z3 = ZipList2 [(+9), (*2), (+8)]
z3' = ZipList2 [1..3]
-- λ> z3 <*> z3'
-- ZipList2 [10,4,11]

z3r = ZipList2 (repeat 1)
-- λ> z3 <*> z3r
-- ZipList2 [10,2,9]


-- Exercise: Validations on Either
data Validation e a =
    Failure' e
  | Success' a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure' e) = Failure' e
  fmap f (Success' a) = Success' (f a)

instance Monoid e => Applicative (Validation e) where
  pure = Success'
  (<*>) (Failure' e) (Success' _) = Failure' e
  (<*>) (Success' _) (Failure' e) = Failure' e
  (<*>) (Failure' e1) (Failure' e2) = Failure' (e1 <> e2)
  (<*>) (Success' f) (Success' a) = Success' (f a)


-- Chapter Exercises
-- 1
data Pair a = Pair a a
  deriving Show

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair f f') (Pair a a') = (Pair (f a) (f' a'))

-- 2
data Two a b = Two a b

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure b = Two mempty b
  (<*>) (Two a f) (Two a' b') = Two (a <> a') (f b')

-- 3
data Three a b c = Three a b c

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure c = Three mempty mempty c
  (<*>) (Three a b f) (Three a' b' c') = Three (a <> a') (b <> b') (f c')

-- 4
data Three' a b = Three' a b b

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance Monoid a => Applicative (Three' a) where
  pure b = Three' mempty b b
  (<*>) (Three' a f g) (Three' a' b' c') = Three' (a <> a') (f b') (g c')

-- 5
data Four a b c d = Four a b c d

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure d = Four mempty mempty mempty d
  (<*>) (Four a b c f) (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (f d')

-- 6
data Four' a b = Four' a a a b

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance Monoid a => Applicative (Four' a) where
  pure d = Four' mempty mempty mempty d
  (<*>) (Four' a b c f) (Four' a' b' c' d') = Four' (a <> a') (b <> b') (c <> c') (f d')


-- Combinations
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

-- liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

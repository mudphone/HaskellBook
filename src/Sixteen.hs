{-# LANGUAGE FlexibleInstances #-}
module Sixteen where

import GHC.Arr
import Test.QuickCheck

a = fmap (+1) $ read "[1]" :: [Int]

b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

c = (*2) . (\x -> x - 2)

d = ((return '1' ++) . show) . (\x -> [x, 1..3])

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap read (fmap ("123"++) (fmap show ioi))
    in fmap (*3) changed


-- 16.9 QuickChecking Functor instances

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)


-- Identity a
newtype Identity a = Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen


-- Pair a
data Pair a = Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

pairGen :: (Arbitrary a) => Gen (Pair a)
pairGen = do
  a <- arbitrary
  b <- arbitrary
  return $ Pair a b

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = pairGen


-- Two a b
data Two a b = Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  a <- arbitrary
  b <- arbitrary
  return $ Two a b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen


-- Three a b c
data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
threeGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return $ Three a b c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = threeGen


-- Three' a b
data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

threePrimeGen :: (Arbitrary a, Arbitrary b) => Gen (Three' a b)
threePrimeGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return $ Three' a b c

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = threePrimeGen


-- Four a b c d
data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

fourGen :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
fourGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return $ Four a b c d

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b c d) where
  arbitrary = fourGen


-- Four' a b
data Four' a b = Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

fourPrimeGen :: (Arbitrary a, Arbitrary b) => Gen (Four' a b)
fourPrimeGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return $ Four' a b c d

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = fourPrimeGen


main :: IO ()
main = do
  quickCheck $ \x -> functorIdentity (x :: Identity Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Identity Int)
  quickCheck $ \x -> functorIdentity (x :: Pair Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Pair Int)
  quickCheck $ \x -> functorIdentity (x :: Two Char Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Two Char Int)
  quickCheck $ \x -> functorIdentity (x :: Three Char Char Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Three Char Char Int)
  quickCheck $ \x -> functorIdentity (x :: Three' Char Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Three' Char Int)
  quickCheck $ \x -> functorIdentity (x :: Four Char Char Char Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Four Char Char Char Int)
  quickCheck $ \x -> functorIdentity (x :: Four' Char Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Four' Char Int)


-- Possibly
data Possibly a =
    LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap f (Yeppers a) = Yeppers (f a)
  fmap _ LolNope = LolNope


-- Sum
data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Second b) = Second (f b)
  fmap _ (First a) = First a

--
-- Exercises
--

--
data BoolAndMaybeSomethingElse a = Fetish | Truish a
  deriving (Eq, Show)

instance Functor BoolAndMaybeSomethingElse where
  fmap _ Fetish = Fetish
  fmap f (Truish a) = Truish (f a)


-- 1
data Sum1 b a = First1 a | Second1 b
  deriving (Eq, Show)

instance Functor (Sum1 b) where
  fmap f (First1 a) = First1 (f a)
  fmap f (Second1 b) = Second1 b

-- 2
data Company a b c = DeepBlue a b | Something c
  deriving (Eq, Show)

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-- 3
data More b a =
    L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More a) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'


-- 1
data Quant a b =
    Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

-- 2
data K a b = K a

instance Functor (K b) where
  fmap _ (K a) = K a

-- 3
newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

newtype K3 a b = K3 a

instance Functor (Flip K3 a) where
  fmap f (Flip (K3 a)) = Flip (K3 (f a))

-- 4
data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

-- 5
data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f2 (LiftItOut b) = LiftItOut (fmap f2 b)

-- 6
data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f2 (DaWrappa b c) = DaWrappa (fmap f2 b) (fmap f2 c)

-- 7
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f2 (IgnoringSomething c d) = IgnoringSomething c (fmap f2 d)

-- 8
data Notorious g o a t =
  Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious b c d) = Notorious b c (fmap f d)

-- 9
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a list) = Cons (f a) (fmap f list)

-- 10
data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats g1 g2 g3) = MoreGoats (fmap f g1) (fmap f g2) (fmap f g3)

-- 11
data TalkToMe a =
    Halt
  | Print  String a
  | Read  (String -> a)
  -- deriving (Eq, Show)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read sta) = Read (f . sta)



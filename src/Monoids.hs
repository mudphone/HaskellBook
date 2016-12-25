module Monoids where

-- Used https://lukleh.github.io/haskell-book-exercises/#_15_14_chapter_exercises
-- To get some hints on Identity and Mem.

import Control.Monad
import Data.Semigroup
import Test.QuickCheck hiding (Failure, Success)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- Trivial
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  Trivial <> Trivial = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool


-- Identity a
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup (Identity a) where
  Identity a <> Identity b = Identity a

instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool


-- Two a b
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two a1 b1 <> Two a2 b2 = Two (a1 <> a2) (b1 <> b2)

instance (Monoid a, Semigroup a, Monoid b, Semigroup b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  a <- arbitrary
  b <- arbitrary
  return $ Two a b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen

type TwoAssoc a b = (Two a b) -> (Two a b) -> (Two a b) -> Bool


-- BoolConj
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj b1 <> BoolConj b2 = BoolConj $ b1 && b2

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

instance Arbitrary BoolConj where
  arbitrary = elements [(BoolConj True), (BoolConj False)]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool 


-- BoolDisj
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj b1 <> BoolDisj b2 = BoolDisj $ b1 || b2

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

instance Arbitrary BoolDisj where
  arbitrary = elements [(BoolDisj True), (BoolDisj False)]

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool


-- Combine
newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (\x -> (f x) <> (g x))

instance (Monoid b, Semigroup b) => Monoid (Combine a b) where
  mempty  = Combine (\_ -> mempty)
  mappend = (<>)
  

-- Comp
newtype Comp a = Comp (a -> a)

instance (Semigroup b) => Semigroup (Comp b) where
  (Comp f) <> (Comp g) = Comp (f . g)

instance (Monoid a, Semigroup a) => Monoid (Comp a) where
  mempty = Comp id
  mappend = (<>)


-- Mem
newtype Mem s a =
  Mem { runMem :: s -> (a,s) }

instance (Semigroup a) => Semigroup (Mem s a) where
  (Mem f) <> (Mem g) = Mem (\s -> let (a1, s1) = (g s)
                                      (a2, s2) = (f s1)
                                   in ((a1 <> a2), s2))

instance (Monoid a, Semigroup a) => Monoid (Mem s a) where
  mempty = Mem (\s -> (mempty, s))
  mappend = (<>)

f' = Mem $ \s -> ("hi", s + 1)

mainMem = do
  print $ runMem (f' <> mempty) 0
  print $ runMem (mempty <> f') 0
  print $ (runMem mempty 0 :: (String, Int))
  print $ runMem (f' <> mempty) 0 == runMem f' 0
  print $ runMem (mempty <> f') 0 == runMem f' 0


main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (semigroupAssoc :: IdentityAssoc [Int])
  quickCheck (monoidLeftIdentity :: Identity Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Identity Trivial -> Bool)
  quickCheck (semigroupAssoc :: TwoAssoc Trivial (Identity [Int]))
  quickCheck (monoidLeftIdentity :: Two Trivial (Identity Trivial) -> Bool)
  quickCheck (monoidRightIdentity :: Two Trivial (Identity Trivial) -> Bool)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)  
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)

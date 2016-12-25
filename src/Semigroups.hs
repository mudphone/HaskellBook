module Semigroups where

import Control.Monad
import Data.Semigroup
import Test.QuickCheck hiding (Failure, Success)

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  Trivial <> Trivial = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- Identity a
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup (Identity a) where
  Identity a <> Identity b = Identity a

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

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  a <- arbitrary
  b <- arbitrary
  return $ Two a b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen

type TwoAssoc a b = (Two a b) -> (Two a b) -> (Two a b) -> Bool


-- Three a b c
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  Three a1 b1 c1 <> Three a2 b2 c2 = Three (a1 <> a2) (b1 <> b2) (c1 <> c2)

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
threeGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return $ Three a b c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = threeGen

type ThreeAssoc a b c = (Three a b c) -> (Three a b c) -> (Three a b c) -> Bool


-- Four a b c d
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  Four a1 b1 c1 d1 <> Four a2 b2 c2 d2 =
    Four (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)

fourGen :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
fourGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return $ Four a b c d

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = fourGen

type FourAssoc a b c d = (Four a b c d) -> (Four a b c d) -> (Four a b c d) -> Bool


-- BoolConj
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj b1 <> BoolConj b2 = BoolConj $ b1 && b2

instance Arbitrary BoolConj where
  arbitrary = elements [(BoolConj True), (BoolConj False)]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool 


-- BoolDisj
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj b1 <> BoolDisj b2 = BoolDisj $ b1 || b2

instance Arbitrary BoolDisj where
  arbitrary = elements [(BoolDisj True), (BoolDisj False)]

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool


-- Or
data Or a b =
    Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  Snd b <> _ = Snd b
  _ <> Snd b = Snd b
  Fst a <> _ = Fst a

orGen :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
orGen = do
  a <- arbitrary
  b <- arbitrary
  elements [Fst a, Snd b]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = orGen

type OrAssoc a b = Or a b -> Or a b -> Or a b -> Bool


-- Combine
newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (\x -> (f x) <> (g x))


-- Validation a b
data Validation a b =
  Failure a | Success b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (Failure a1) <> (Failure a2) = Failure (a1 <> a2)
  (Failure a1) <> _            = Failure a1
  _            <> (Failure a2) = Failure a2
  (Success b1) <> _            = Success b1

validationGen :: (Arbitrary a, Arbitrary b) => Gen (Validation a b)
validationGen = do
  a <- arbitrary
  b <- arbitrary
  elements [Failure a, Success b]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = validationGen

type ValidationAssoc a b = Validation a b -> Validation a b -> Validation a b -> Bool


-- AccumulateRight a b
newtype AccumulateRight a b =
  AccumulateRight (Validation a b)
  deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
  (AccumulateRight (Success b1)) <> (AccumulateRight (Success b2)) = AccumulateRight (Success (b1 <> b2))
  (AccumulateRight (Success b1)) <> _ = AccumulateRight (Success b1)
  _ <> (AccumulateRight (Success b2)) = AccumulateRight (Success b2)
  (AccumulateRight (Failure a1)) <> _ = AccumulateRight (Failure a1)

accumulateRightGen :: (Arbitrary a, Arbitrary b) => Gen (AccumulateRight a b)
accumulateRightGen = do
  a <- arbitrary
  b <- arbitrary
  elements [(AccumulateRight (Failure a)), (AccumulateRight (Success b))]

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = accumulateRightGen

type AccumulateRightAssoc a b = AccumulateRight a b -> AccumulateRight a b -> AccumulateRight a b -> Bool


-- AccumulateBoth a b
newtype AccumulateBoth a b =
  AccumulateBoth (Validation a b)
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  (AccumulateBoth (Failure a1)) <> (AccumulateBoth (Failure a2)) = AccumulateBoth (Failure (a1 <> a2))
  (AccumulateBoth (Success b1)) <> (AccumulateBoth (Success b2)) = AccumulateBoth (Success (b1 <> b2))
  (AccumulateBoth (Failure a1)) <> _ = AccumulateBoth (Failure a1)
  _ <> (AccumulateBoth (Failure a2)) = AccumulateBoth (Failure a2)
  
accumulateBothGen :: (Arbitrary a, Arbitrary b) => Gen (AccumulateBoth a b)
accumulateBothGen = do
  a <- arbitrary
  b <- arbitrary
  elements [(AccumulateBoth (Failure a)), (AccumulateBoth (Success b))]

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = accumulateBothGen

type AccumulateBothAssoc a b = AccumulateBoth a b -> AccumulateBoth a b -> AccumulateBoth a b -> Bool


main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc [Int])
  quickCheck (semigroupAssoc :: TwoAssoc Trivial (Identity [Int]))
  quickCheck (semigroupAssoc :: ThreeAssoc Trivial
                                           (Identity [Int])
                                           (Two Trivial (Identity [Int])))
  quickCheck (semigroupAssoc :: FourAssoc Trivial
                                          (Identity [Int])
                                          (Two Trivial (Identity [Int]))
                                          (Three Trivial
                                                 (Identity [Int])
                                                 (Two Trivial (Identity [Int]))))
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc Int Int)
  quickCheck (semigroupAssoc :: ValidationAssoc Trivial Int)
  quickCheck (semigroupAssoc :: AccumulateRightAssoc Int Trivial)
  quickCheck (semigroupAssoc :: AccumulateBothAssoc [Int] [Int])

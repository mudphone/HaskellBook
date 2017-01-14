{-# LANGUAGE InstanceSigs #-}

module TwentyTwo where

import Control.Applicative (liftA2)
import Data.Char

-- Short Exercise: Warming Up
cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = (,) <$> rev <*> cap

tupledM :: [Char] -> ([Char], [Char])
tupledM = do
  r <- rev
  c <- cap
  return (c, r)

tupledBind :: [Char] -> ([Char], [Char])
tupledBind = cap >>= \x1 -> rev >>= \x2 -> return (x1, x2)


-- Exercise: Ask
newtype Reader r a = Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id


-- Exercise: Reading Comprehension
-- 1
newtype HumanName = HumanName String
  deriving (Eq, Show)

newtype DogName = DogName String
  deriving (Eq, Show)

newtype Address = Address String
  deriving (Eq, Show)

data Person = Person {
                humanName :: HumanName
              , dogName :: DogName
              , address :: Address
              } deriving (Eq, Show)

data Dog = Dog {
             dogsName :: DogName
           , dogsAddress :: Address
           } deriving (Eq, Show)

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 g fa fb = g <$> fa <*> fb

pers :: Person
pers = Person (HumanName "Big Bird")
              (DogName "Barkley")
              (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen")
               (DogName "Papu")
               (Address "Austin")

getDogR2 :: Person -> Dog
getDogR2 = myLiftA2 Dog dogName address

-- λ> getDogR2 pers
-- Dog {dogsName = DogName "Barkley", dogsAddress = Address "Sesame Street"}
-- λ> getDogR2 chris
-- Dog {dogsName = DogName "Papu", dogsAddress = Address "Austin"}
              

-- 2
asks :: (r -> a) -> Reader r a
asks f = Reader f


-- 3
instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ (f . ra)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ (\_ -> a)

  (<*>) :: Reader r (a -> b)
        -> Reader r a
        -> Reader r b
  (Reader rab) <*> (Reader ra) =
    Reader $ \r -> (rab r) (ra r)

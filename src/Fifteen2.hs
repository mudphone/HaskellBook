module Fifteen2 where

import Control.Monad
import Data.Monoid
import Test.QuickCheck

data Bull =
    Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [ (1, return Fools)
              , (1, return Twoo) ]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a


data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada (Only a) = (Only a)
  mappend (Only a) Nada = (Only a)
  mappend (Only a) (Only b) = (Only (mappend a b))


--
-- Exercise: Maybe Another Monoid
--
newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (First' a) where
     arbitrary = frequency [(1, return $ First' Nada),
                            (1, fmap (First' . Only) arbitrary)]

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' Nada)     (First' Nada)     = First' Nada
  mappend (First' (Only a)) _                 = First' (Only a)
  mappend _                 (First' (Only a)) = First' (Only a)

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

main :: IO ()
main = do
  -- quickCheck (monoidAssoc :: BullMappend)
  -- quickCheck (monoidLeftIdentity :: Bull -> Bool)
  -- quickCheck (monoidRightIdentity :: Bull -> Bool)
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)

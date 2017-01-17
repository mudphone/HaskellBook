module ReaderPractice where

import Control.Applicative
import Data.Maybe

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

-- lookup :: Eq a => a -> [(a, b)] -> Maybe b

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup 3 $ zip x y

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = lookup 6 $ zip y z

-- zip x and y using 4 as the lookup key
zs :: Maybe Integer
zs = lookup 4 $ zip x y

-- now zip x and z using a variable lookup key
z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

-- Have x1 make a tuple of xs and ys
x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

-- and x2 make a tuple of ys and zs
x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (a, a)
  where a = z' n


-- 22.11 Chapter Exercises
uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a, b) = f a b

summed :: Num c => (c, c) -> c
summed = uncurry' (+)

bolt :: Integer -> Bool
bolt = (&&) <$> (>3) <*> (<8)

fromMaybe' :: a -> Maybe a -> a
fromMaybe' _ (Just x) = x
fromMaybe' y Nothing = y

main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(>3), (<8), even] 7

-- sequenceA
-- sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
-- sequenceA [(>3), (<8), even] 7
-- f ~ (->) a and t ~ []

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

-- s' = Just 15
s' = summed <$> ((,) <$> xs <*> ys)

main' :: IO ()
main' = do
  print $ foldr (&&) True $ sequA 7
  print $ sequA $ fromMaybe' 0 s'
  print $ bolt  $ fromMaybe' 0 ys

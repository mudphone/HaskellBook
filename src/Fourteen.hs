module Fourteen where

import Data.Char (toUpper)
import Data.List (sort)

import Test.Hspec
import Test.QuickCheck

half :: (Fractional a) => a -> a
half x = x / 2

halfIdentity = (*2) . half

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

plusAssociative x y z = x + (y + z) == (x + y) + z
plusCommutative x y = x + y == y + x
multAssociative x y z = x * (y * z) == (x * y) * z
multCommutative x y = x * y == y * x
expAssociative x y z = x ^ (y ^ z) == (x ^ y) ^ z
expCommutative x y = x ^ y == y ^ x

square x = x * x
squareIdentity = square . sqrt

twice f = f . f
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

main :: IO ()
main = hspec $ do
  it "half x is always equal to 0.5 * x" $ do
    property $ \x -> half x == (x :: Float) * 0.5
  it "halfIdentity x is always equal to x" $ do
    property $ \x -> halfIdentity x == (x :: Float)
  it "listOrdered . sort $ xs is always true" $ do
    property $ \xs -> length xs > 1 ==> listOrdered . sort $ (xs :: [Int])
  it "plusAssociative is always true" $ do
    property $ \x y z -> plusAssociative (x :: Int) (y :: Int) (z :: Int)
  it "plusCommutative is always true" $ do
    property $ \x y -> plusCommutative (x :: Int) (y :: Int)
  it "multAssociative is always true" $ do
    property $ \x y z -> multAssociative (x :: Int) (y :: Int) (z :: Int)
  it "multCommutative is always true" $ do
    property $ \x y -> multCommutative (x :: Int) (y :: Int)
  it "(quot x y) * y + rem x y is always equal to x" $ do
    property $ \x y -> y > 0 ==> (quot (x :: Int) (y :: Int)) * y + (rem x y) == x
  it "(div x y) * y + mod x y is always equal to x" $ do
    property $ \x y -> y > 0 ==> (div (x :: Int) (y :: Int)) * y + (mod x y) == x
  -- it "^ is associative" $ do
  --   property $ \x y z -> expAssociative (x :: Int) (y :: Int) (z :: Int)
  -- it "^ is commutative" $ do
  --   property $ \x y -> expCommutative (x :: Int) (y :: Int)
  it "reverse list twice is always equal to identity" $ do
    property $ \xs -> (reverse . reverse) xs == id (xs :: [Int])
  it "f $ g $ h x is always equal to f (g (h x))" $ do
    property $ \x a b c -> b > 0 ==> ((+ (a :: Float)) $ (/ (b :: Float)) $ (* (c :: Float)) (x :: Float)) == (((x * c) / b) + a) 
  -- it "foldr (:) is always equal to (++)" $ do
  --   property $ \xs ys -> foldr (:) (xs :: [Int]) (ys :: [Int]) == (++) xs ys
  it "foldr (++) [] is always equal to concat" $ do
    property $ \xxs -> foldr (++) [] (xxs :: [[Int]]) == concat xxs
  it "length (take n xs) is always equal to n" $ do
    property $ \n xs -> n >= 0 && length xs > 0 && n <= length xs ==> length (take (n :: Int) (xs :: [Int])) == n
  it "read . show x is always equal to x" $ do
    property $ \x -> (read . show) (x :: Int) == x
  -- it "fails" $ do
  --   property $ \x -> squareIdentity (x :: Float) == x
  it "capitalizeWord is idempotent" $ do
    property $ \word -> (capitalizeWord (word :: String) == (twice capitalizeWord) word) && (capitalizeWord word == (fourTimes capitalizeWord) word) 
  it "sort is idempotent" $ do
    property $ \xs -> (sort (xs :: [Int]) == (twice sort) xs) && (sort xs == (fourTimes sort) xs)

data Fool = Fulse | Frue
  deriving (Eq, Show)

foolGenEqual :: Gen Fool
foolGenEqual = oneof [return Fulse,
                 return Frue]

foolGenFulsey :: Gen Fool
foolGenFulsey = frequency [(2, return Fulse),
                           (1, return Frue)]

instance Arbitrary Fool where
--   arbitrary = foolGenEqual
  arbitrary = foolGenFulsey


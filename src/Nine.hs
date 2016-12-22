module Nine where

import Data.Char

onlyCaps :: [Char] -> [Char]
onlyCaps = filter isUpper 

capIt :: [Char] -> [Char]
capIt (x:xs) = (toUpper x) : xs 

woot :: [Char] -> [Char]
woot (x:[]) = [toUpper x]
woot (x:xs) = (toUpper x) : (woot xs)

theHead :: [Char] -> Char
theHead = toUpper . head

theHead' :: [Char] -> Char
theHead' x = toUpper (head x)

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x then True else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny p (x:xs ) = if (p x) then True else myAny p xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs) = if e == x then True else myElem e xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' e = any (== e)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain [] = []
squishAgain (x:xs) = squishMap (\y -> [y])  x ++ squishAgain xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy compFun (x:xs) = foldl cc x xs
  where cc x y = if compFun y x == GT then y else x

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy compFun (x:xs) = foldl cc x xs
  where cc x y = if compFun y x == LT then y else x

myMinimumBy' :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy' compFun = myMaximumBy (flip compFun)

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare

myMinimum' :: (Ord a) => [a] -> a
myMinimum' = myMinimumBy' compare

module Twelve where
import Data.Char (toUpper)
import Data.List (intersperse, unfoldr)

notThe :: String -> Maybe String
notThe w
  | w == "the" || w == "The" = Nothing
  | otherwise = Just w

removeThe :: Maybe String -> String
removeThe (Just x) = x
removeThe Nothing = "a"

replaceThe :: String -> String
replaceThe s = concat $ intersperse " " $ fmap (removeThe . notThe) w
  where w = words s

isVowel :: Char -> Bool
isVowel letter = elem (toUpper letter) "AEIOU"

counter :: (Bool, Integer) -> String -> (Bool, Integer)
counter (_, cnt) "the" = (True, cnt)
counter (True, cnt) (w:ws) = if isVowel w then (False, cnt+1) else (False, cnt)
counter (False, cnt) _ = (False, cnt)

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = (\(_, cnt) -> cnt) $ foldl counter (False, 0) w
  where w = words s

countVowels :: String -> Integer
countVowels s = (toInteger . length) $ filter isVowel s

newtype Word' =
  Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s = go s (0, 0)
  where go [] (consonants, vowels) = if vowels > consonants then Nothing else Just (Word' s)
        go (x:xs) (consonants, vowels)
          | isVowel x = go xs (consonants, vowels + 1)
          | otherwise = go xs (consonants + 1, vowels)
  
data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nat) = 1 + (natToInteger nat)

integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i < 0 = Nothing
  | otherwise = Just (go i)
    where go int
            | int == 0 = Zero
            | otherwise = (Succ (go (int - 1)))

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just _) = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a m = mayybee a id m 

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList (Just a) = [a]
maybeToList Nothing = []

catMaybes :: [Maybe a] -> [a]
catMaybes ms = foldr f [] ms
  where f (Just a) b = [a] ++ b
        f Nothing b = b

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr f (Just [])
  where f _ Nothing = Nothing
        f Nothing _ = Nothing
        f (Just a) (Just b) = Just ([a] ++ b) 

lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where f (Left a) acc = [a] ++ acc
        f (Right _) acc = acc

rights' :: [Either a b] -> [b]
rights' = foldr f []
  where f (Left _) acc = acc
        f (Right b) acc = [b] ++ acc

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr f ([], [])
  where f (Left a) (lefts, rights) = ([a] ++ lefts, rights)
        f (Right b) (lefts, rights) = (lefts, [b] ++ rights)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right b) = Just (f b) 

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ g (Right b) = g b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' _ (Left _) = Nothing
eitherMaybe'' g r@(Right b) = Just (either' (\_ -> g b) g r)

myIterate :: (a -> a) -> a -> [a]
myIterate f a = [a] ++ (myIterate f (f a))

unf :: (Ord b, Num b) => b -> Maybe (b, b)
unf n
  | n > 10 = Nothing
  | otherwise = Just (n, n + 1)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = go $ f b
  where go Nothing = []
        go (Just (a, nextB)) = [a] ++ (go $ f nextB)

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\x -> Just (x, f x)) x

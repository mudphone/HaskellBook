module Twenty where

import Data.Monoid

-- Exercises: Library Functions
-- 1
sum :: (Monoid a, Foldable t, Num a) => t a -> a
sum = foldMap (+ mempty)

-- 2
product :: (Monoid a, Foldable t, Num a) => t a -> a
product = foldMap (* mempty)

-- 3
elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x xs = foldr (\a acc -> acc || a == x) False xs

-- 4
minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum xs = Just $ foldr1 min' xs
  where min' x y = case compare x y of
                        GT -> y
                        _  -> x

-- 5
maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum xs = Just $ foldr1 max' xs
  where max' x y = case compare x y of
                        GT -> x
                        _  -> y

-- 6
null :: (Foldable t) => t a -> Bool
null = foldr (\a acc -> False) True

-- 7
length :: (Foldable t) => t a -> Int
length = foldr (\a acc -> 1 + acc) 0

-- 8
toList :: (Foldable t) => t a -> [a]
toList = foldr (\a acc -> [a] ++ acc) []

-- 9
fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap (mappend mempty)

-- 10
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\a acc -> (f a) <> acc) mempty


-- 20.6 Chapter Exercises
-- 1
data Constant a b = Constant a

instance Foldable (Constant a) where
  foldMap _ _ = mempty

-- 2
data Two a b = Two a b

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b

-- 3
data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

-- 4
data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' a b c) = f b <> f c

-- 5
data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' a b c d) = f b <> f c <> f d

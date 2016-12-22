module Seven where

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = d
  where xLast  = x `divMod` 10
        (_, d) = (fst xLast) `divMod` 10

hunsD :: Integral a => a -> a
hunsD x = d2
  where d      = x `divMod` 100
        (_, d2) = (fst d) `divMod` 10

foldBool :: a -> a -> Bool -> a
foldBool x y b =
  case b of
    True -> x
    False -> y

foldBool' :: a -> a -> Bool -> a
foldBool' x y b
  | b     = x
  | not b = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) =
  ((f a), c)

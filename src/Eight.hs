module Eight where

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

-- fill in the types

-- flip :: (a -> b -> c) -> b -> a -> c

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
         | n < d     = (count, n)
         | otherwise = go (n - d) d (count + 1)

rSum :: (Eq a, Num a) => a -> a
rSum x = go 1 x
  where go n x
         | x == 0     = 0
         | x == n     = n
         | otherwise  = n + (go (n + 1) x)
        
rMul :: (Integral a) => a -> a -> a
rMul x y = go x y
  where go x y
         | x == 0    = 0
         | otherwise = y + (go (x - 1) y) 

data DividedResult a =
    Result (a, a)
  | DividedByZero
  deriving Show

dividedBy' :: Integral a => a -> a -> DividedResult a
dividedBy' num denom = go num denom 0 1
  where go n d count sign
         | d == 0    = DividedByZero
         | n < 0     = go (-n) d count (-sign)
         | d < 0     = go n (-d) count (-sign)
         | n < d     = Result ((sign * count), n)
         | otherwise = go (n - d) d (count + 1) sign

mc91 :: Integral a => a -> a
mc91 x = go x
  where go n
         | n >  100 = n - 10
         | n <= 100 = go (go (n + 11))
  

{-# LANGUAGE InstanceSigs #-}
module TwentyThree where

import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

-- Exercises: Roll Your Own
-- 1
rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= 20 = count
          | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
            in go (sum + die) (count + 1) nextGen

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= n = count
          | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
            in go (sum + die) (count + 1) nextGen

-- 2
rollsCountLogged :: Int -> StdGen -> (Int, [Int])
rollsCountLogged n g = go 0 (0, []) g
  where go :: Int -> (Int, [Int]) -> StdGen -> (Int, [Int])
        go sum st gen
          | sum >= n = st
          | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
                (count, log) = st
            in go (sum + die) (count + 1, log ++ [die]) nextGen

-- Moi
newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s -> let (a, s1) = g s
                               in (f a, s1)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi (\s -> (a, s))

  (<*>) :: Moi s (a -> b)
        -> Moi s a
        -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \s -> let (a, s1) = g s
                                        (ab, s2) = f s1
                                    in (ab a, s2)

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a
        -> (a -> Moi s b)
        -> Moi s b
  (Moi f) >>= g = Moi $ \s -> let (a, s1) = f s
                                  (Moi sb) = g a
                              in sb s1

-- Fizzbuzz Differently
fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Fizz"
           | n `mod` 3  == 0 = "Buzz"
           | otherwise       = show n

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo from to =
  let xs =  (\x -> to - x + 1) <$> [from..to]
  in fizzbuzzList xs
                           
main :: IO ()
main = do
  mapM_ putStrLn $ reverse $ fizzbuzzList [1..10]
  mapM_ putStrLn $ fizzbuzzFromTo 1 10


-- 23.8 Chapter Exercises
-- 1
get' :: State s s
get' = state (\x -> (x, x))

-- 2
put' :: s -> State s ()
put' s = state $ (\_ -> ((), s))

-- 3
exec' :: State s a -> s -> s
exec' ssa s = let sa = runState ssa
              in snd $ sa s

-- 4
eval' :: State s a -> s -> a
eval' ssa s = let sa = runState ssa
              in fst $ sa s

-- 5
myModify :: (s -> s) -> State s ()
myModify ss = state $ \s -> ((), ss s)

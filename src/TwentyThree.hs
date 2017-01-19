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

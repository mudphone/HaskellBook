module TwentyTwo where

import Control.Applicative (liftA2)
import Data.Char

-- Short Exercise: Warming Up
cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = (,) <$> rev <*> cap

tupledM :: [Char] -> ([Char], [Char])
tupledM = do
  r <- rev
  c <- cap
  return (c, r)

tupledBind :: [Char] -> ([Char], [Char])
tupledBind = cap >>= \x1 -> rev >>= \x2 -> return (x1, x2)

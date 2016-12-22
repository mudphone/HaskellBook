module Cipher where

import Control.Monad(forever)
import Data.Char

rotorNum = 13
unrotorNum = 26 - rotorNum

cipherBase :: Char -> Int
cipherBase x = if (ord x) < 97 then 65 else 97

encode :: Char -> Int -> Char
encode x n =
  (chr . (\x -> (mod x 26) + base) . (+ n) . (\x -> x - base) . ord) x
  where base = cipherBase x

caesar' :: [Char] -> Int -> [Char]
caesar' [] _ = []
caesar' (x:xs) n = [(encode x n)] ++ (caesar' xs n)

caesar :: [Char] -> [Char]
caesar xs = caesar' xs rotorNum

unCaesar :: [Char] -> [Char]
unCaesar xs = caesar' xs unrotorNum

vigOrder :: Char -> Int
vigOrder c = (ord c) - (cipherBase c)

vigenere :: [Char] -> [Char] -> [Char]
vigenere word pass = go word (take (length word) (cycle pass))
  where go (' ':ws) pass = [' '] ++ (go ws pass)
        go (w:ws) (p:ps) = [encode w (vigOrder p)] ++ (go ws ps)
        go [] _ = []

unVigenere :: [Char] -> [Char] -> [Char]
unVigenere word pass = go word (take (length word) (cycle pass))
  where go (' ':ws) pass = [' '] ++ (go ws pass)
        go (w:ws) (p:ps) = [(encode w (26 - (vigOrder p)))] ++ (go ws ps)
        go [] _ = []

runVigenere :: IO ()
runVigenere = forever $ do
  putStr "Enter a word to vigenere: "
  word <- getLine
  putStr "Enter pass: "
  pass <- getLine
  let coded = vigenere word pass
  putStrLn $ "Output: " ++ coded

runCaesar :: IO ()
runCaesar = forever $ do
  putStr "Enter a word to caesar: "
  word <- getLine
  let coded = caesar word
  putStrLn $ "Output: " ++ coded

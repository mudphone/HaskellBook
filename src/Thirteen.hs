module Thirteen where

import Control.Monad (forever)
import Data.Char (toLower)
import System.Exit (exitSuccess)

lowercase :: String -> String
lowercase [] = []
lowercase (x:xs) = [toLower x] ++ (lowercase xs) 

clean :: String -> String
clean = (lowercase . filter (\x -> elem x $ ['a'..'z']++['A'..'Z']))

palindrome :: IO ()
palindrome = forever $ do
  putStr "Gimme a palindrome: "
  line1 <- getLine
  let cleanLine = clean line1
  case (cleanLine == reverse cleanLine) of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess

-- #4

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Eq, Show)

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person

mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
                       "Name was: " ++ show name ++
                       " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStr "Gimme a name: "
  name <- getLine
  putStr "Gimme an age: "
  age <- getLine
  let intAge = read age :: Integer
  case mkPerson name intAge of
    Right person -> do
      putStr "Yay! Successfully got a person: "
      putStrLn $ show person
    Left error -> do
      putStr "Error: "
      putStrLn $ show error

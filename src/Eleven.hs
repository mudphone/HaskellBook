module Eleven where

import Data.Char
import Data.List (intersperse, elemIndex, elemIndices, maximumBy, nub)
import Data.List.Split (splitOn)
import Data.Char (isSpace)

data Weekday =
    Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday

f :: Weekday -> String
f Friday = "Miller Timer"

g :: [a] -> a
g xs = xs !! (length xs - 1)

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
-- isSubsequenceOf [x] (y:ys) = x == y
isSubsequenceOf (x:xs) [] = False
isSubsequenceOf word@(x:xs) (y:ys) =
  if x == y then isSubsequenceOf xs ys else isSubsequenceOf word ys

capitalizeWord :: String -> String
capitalizeWord (x:xs) = [toUpper x] ++ xs

capitalizeWords :: String -> [(String, String)]
capitalizeWords = go . words
  where go (w:ws) = [(w, capitalizeWord w)] ++ go ws
        go [] = []

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

capitalizeParagraph :: String -> String
capitalizeParagraph = concat . (intersperse ". ") . map (capitalizeWord . trim) . (splitOn ". ")

type Digit = Char
type Label = String
type Presses = Int
type DaButton = (Digit, Label) 
data DaPhone = DaPhone [DaButton]

telephone :: DaPhone
telephone = DaPhone [('1', ""),
                     ('2', "ABC"),
                     ('3', "DEF"),
                     ('4', "GHI"),
                     ('5', "JKL"),
                     ('6', "MNO"),
                     ('7', "PQRS"),
                     ('8', "TUV"),
                     ('9', "WXYZ"),
                     ('*', "^"),
                     ('0', "+ _"),
                     ('#', ".,")]

convo :: [String]
convo =
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever tasted alcohol lol",
   "Lol ya",
   "Wow ur cool haha. Ur turn",
   "Ok. Do u think I am pretty Lol",
   "Lol ya",
   "Haha thanks just making sure rofl ur turn"]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone buttons) c = r ++ [(digit, (labelToPresses label u))]
  where u = toUpper c
        r = if isUpper c then [('*', 1)] else []
        (digit, label) = head $ filter (\(dig, lbl) ->
                                            elem u (lbl++[dig])) buttons
        labelToPresses lbl c = (\(Just n) -> n + 1) $ (elemIndex c (label ++ [digit]))

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone s = s >>= (reverseTaps phone)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\(_, p) a -> p + a) 0

mostPopularLetter' :: String -> (Char, Int)
mostPopularLetter' s = maximumBy (\(_, a) (_, b) ->
                                     compare a b) counts
  where idx = nub s
        counts = map (\x -> (x, length $ elemIndices x s)) idx

mostPopularLetter :: String -> Char
mostPopularLetter = (\(x, _) -> x) . mostPopularLetter'

coolestLtr :: [String] -> Char
coolestLtr = (\(c, _) -> c) . (maximumBy (\(_, a) (_, b) -> compare a b)) . (map mostPopularLetter')

coolestWord :: [String] -> String
coolestWord xs = (\(x, _) -> x) $ maximumBy (\(_, a) (_, b) -> compare a b) counts
  where allWords = concatMap words xs
        idx = nub allWords
        counts = map (\x -> (x, length $ elemIndices x allWords)) idx

module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n = ["zero", "one", "two", "three", "four",
                 "five", "six", "seven", "eight", "nine"] !! n
           
digits :: Int -> [Int]
digits n = go n []
  where go x acc
         | x < 10  = [x] ++ acc
         | x >= 10 = go (div x 10) ([(mod x 10)] ++ acc)

wordNumber :: Int -> String
wordNumber n = concat (intersperse "-" (map digitToWord (digits n)))

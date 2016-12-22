module Ten where

stops = "pbtdkg"
vowels = "aeiou"

stopVowelStop :: [Char] -> [Char] -> [[Char]]
stopVowelStop stops vowels = [[x, y, z] | x <- stops, y <- vowels, z <- stops]

noPs :: [Char] -> [Char] -> [[Char]]
noPs stops vowels = [[x, y, z] | x <- stops, y <- vowels, z <- stops, x /= 'p']

onlyPs :: [Char] -> [Char] -> [[Char]]
onlyPs stops vowels = [[x, y, z] | x <- stops, y <- vowels, z <- stops, x == 'p']

nouns = ["house", "cane", "sugar", "wave"]
verbs = ["blow", "swim", "argue", "fart"]

nounVerbNoun :: [[Char]] -> [[Char]] -> [[Char]]
nounVerbNoun nouns verbs = [x ++ " " ++ y ++ " " ++ z | x <- nouns, y <- verbs, z <- nouns]

avgLetters :: Fractional a => [Char] -> a
avgLetters sentence = numLetters / numWords
  where wordList = words sentence
        numWords = realToFrac (length wordList)
        numLetters  = realToFrac (sum (map length wordList))
        
myOr :: [Bool] -> Bool
myOr = foldr (\a b ->
                if a then True else b) False

myOr' :: [Bool] -> Bool
myOr' = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny p = foldr (\a b ->
                    if p a then True else b) False 

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' p = foldr (\a b -> (p a) || b) False

myElem :: Eq a => a -> [a] -> Bool
myElem e = foldr (\a b -> a == e || b) False

myReverse :: [a] -> [a]
myReverse = foldr (\a b -> b ++ [a]) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> [f a] ++ b) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr (\a b ->
                      if p a then [a] ++ b else b) []

squish :: [[a]] -> [a]
squish = foldr (\a b -> a ++ b) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> f a ++ b) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy compFn = foldr1 (\a b ->
                              if (compFn a b) == GT then a else b)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy compFn = foldr1 (\a b ->
                               if (compFn a b) == LT then a else b)

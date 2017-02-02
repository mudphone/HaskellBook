{-# LANGUAGE BangPatterns #-}
module TwentySeven where

-- Exercises: Evaluate
-- 1
x1 = const 1 undefined
-- => 1

-- 2
x2 = const undefined 1
-- => Exception: Prelude.undefined

-- 3
x3 = flip const undefined 1
-- => 1

-- 4
x4 = flip const 1 undefined
-- => Exception: Prelude.undefined

-- 5
x5 = const undefined undefined
-- => Exception: Prelude.undefined

-- 6
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
x6 = foldr const 'z' ['a'..'e']
-- => 'a'

-- 7
x7 = foldr (flip const) 'z' ['a'..'e']
-- => 'z'


-- Chapter Exercises
-- What will :sprint output?
-- 1
y1 = 1
-- :sprint y1
-- y1 = _

-- 2
y2 = ['1']
-- :sprint y2
-- y2 = _

-- 3
y3 = [1]
-- :sprint y3
-- y3 = _

-- 4
y4 = 1 :: Int
-- :sprint y4
-- y4 = _

-- 5
f5 = \x -> x
y5 = f5 1
-- :sprint f5
-- f5 = _
-- :sprint y5
-- y5 = _

-- 6
f6 :: Int -> Int; f6 = \x -> x
y6 = f6 1
-- :sprint f6
-- f6 = _
-- :sprint y6
-- y6 = _


-- Will printing this expression result in bottom?
-- 1
z1 = snd (undefined, 1)
-- z1 => 1

-- 2
-- let x = undefined
-- let y = x `seq` 1 in snd (x, y)
-- Exception: Prelude.undefined

-- 3
z3 = length $ [1..5] ++ undefined
-- z3 => Exception: Prelude.undefined

-- 4
z4 = length $ [1..5] ++ [undefined]
-- z4 => 6

-- 5
z5 = const 1 undefined
-- z5 => 1

-- 6
z6 = const 1 (undefined `seq` 1)
-- z6 => 1

-- 7
z7 = const undefined 1
-- z7 => Exception: Prelude.undefined


-- Make the expression bottom
-- 1
x = undefined
y = "blah"
pp !x1 y1 = print (snd (x1, y1))
main = do
  pp x y

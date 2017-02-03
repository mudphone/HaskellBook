module TwentyEight where

-- Chapter Exercises
newtype DList a = DL { unDL :: [a] -> [a] }
  
-- 1
empty :: DList a
empty = DL $ \_ -> []
{-# INLINE empty #-}

-- 2
singleton :: a -> DList a
singleton x = DL $ \_ -> [x]
{-# INLINE singleton #-}

-- 3
toList :: DList a -> [a]
toList d = unDL d [] 
{-# INLINE toList #-}

-- 4 Prepend a single element to a dlist.
infixr `cons`
cons :: a -> DList a -> DList a
cons x xs = DL ((x:) . unDL xs)
{-# INLINE cons #-}

-- 5 Append a single element to a dlist.
infixl `snoc`
snoc :: DList a -> a -> DList a
snoc xs x = DL $ \d -> (unDL xs $ d) ++ [x]
{-# INLINE snoc #-}
                       
-- 6 Append dlists.
append :: DList a -> DList a -> DList a
append x y = DL $ \_ -> (unDL x $ []) ++ (unDL y $ [])


-- A simple queue
data Queue a =
  Queue { enqueue :: [a]
        , dequeue :: [a]
        } deriving (Eq, Show)

-- adds an item
push :: a -> Queue a -> Queue a
push x xs = Queue ([x] ++ enqueue xs) (dequeue xs)

pop :: Queue a -> Maybe (a , Queue a)
pop (Queue [] [])   = Nothing
pop (Queue [] [y])  = Just (y, Queue [] [])
pop (Queue xs [y]) =
  Just (y, Queue (take (n - 1) xs) [xs !! (n - 1)])
  where n = length xs
pop (Queue xs ys)   = Just (last ys, Queue xs (init ys))

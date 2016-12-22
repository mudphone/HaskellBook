module Arith4 where

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTrip' :: (Show a, Read a) => a -> a
roundTrip' a = f a
  where f = read . show

-- print ((roundTrip3 1) :: Int)
roundTrip3 :: (Show a, Read b) => a -> b
roundTrip3 a = read (show a)

main = do
  print (roundTrip 4)
  print (id 4)

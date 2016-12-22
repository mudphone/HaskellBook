module Two
       ( mult1
       ) where

mult1 = x * y
  where x = 5
        y = 6

myx = x + 9001
  where x = 10

myx1 = x * 3 + y
  where x = 3
        y = 1000

myx2 = x * 5
  where y = 10
        x = 10 * 5 + y

myx3 = z / x + y
  where x = 7
        y = negate x
        z = y * 10

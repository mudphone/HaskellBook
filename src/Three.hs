module Three where

main :: IO ()
main = do
  putStrLn greeting
  printSecond
  where greeting = "Yarrrrr"
        printSecond :: IO ()
        printSecond = do
          putStrLn greeting

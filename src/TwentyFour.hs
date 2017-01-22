module TwentyFour where

import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one = char '1'

one' = one >> stop

oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

oneEof = one >> eof >> stop
oneTwoEof = oneTwo >> eof >> stop

parseIntegerEof :: Parser Integer
parseIntegerEof = do
  number <- decimal
  eof
  return number

stringChar (x:xs) = char x >> stringChar xs
stringChar [] = eof

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  pNL "stringChar: 123"
  print $ parseString (stringChar "123") mempty "123"
  pNL "parseReturnNumber: 123"
  print $ parseString parseIntegerEof mempty "123"
  pNL "parseReturnNumber: 123abc"
  print $ parseString parseIntegerEof mempty "123abc"
    

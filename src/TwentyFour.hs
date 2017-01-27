module TwentyFour where

import Text.Trifecta
import Control.Applicative ((<|>))
import Data.Monoid ((<>))
import Data.Char (digitToInt)

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


-- Exercises: 1
data NumberOrString = NOSS String | NOSI Integer
  deriving Show

instance Eq NumberOrString where
  (==) (NOSI i) (NOSI i') = i == i'
  (==) (NOSS s) (NOSS s') = s == s'
  (==) _ _ = False

instance Ord NumberOrString where
  compare (NOSI i) (NOSI i') = compare i i'
  compare (NOSS s) (NOSS s') = compare s s'
  compare (NOSI _) _         = LT
  compare _        (NOSI _)  = GT

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata
  deriving Show

instance Eq SemVer where
  (==) (SemVer ma mi pa re me) (SemVer ma' mi' pa' re' me') =
    ma == ma' && mi == mi' && pa == pa' && re == re' && me == me'

instance Ord SemVer where
  compare (SemVer ma mi pa re _) (SemVer ma' mi' pa' re' _) =
    let c = (compare ma ma') <> (compare mi mi') <> (compare pa pa')
    in case c of
        EQ -> compare re re'
        _  -> c

nos :: Parser NumberOrString
nos = do
  (NOSI <$> decimal) <|> (NOSS <$> some letter)

nosDot :: Parser NumberOrString
nosDot = do
  ns <- nos
  skipMany (oneOf ".")
  return ns

relP :: Parser [NumberOrString]
relP = do
  (char '-' >> many nosDot) <|> (return [])

metP :: Parser [NumberOrString]
metP = do
  (char '+' >> many nosDot) <|> (return [])

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- integer
  char '.'
  minor <- integer
  char '.'
  patch <- integer
  rel <- try relP
  meta <- try metP
  return $ SemVer major minor patch rel meta

testSemVer :: IO ()
testSemVer = do
  pNL "parseSemVer 2.1.1"
  print $ parseString parseSemVer mempty "2.1.1"
  pNL "parseSemVer 1.0.0-x.7.z.92"
  print $ parseString parseSemVer mempty "1.0.0-x.7.z.92"
  pNL "SemVer 2 1 1 [] [] > SemVer 2 1 0 [] [] => True"
  print $ SemVer 2 1 1 [] [] > SemVer 2 1 0 [] []


-- Exersices: 2, 3
parseDigit :: Parser Char
parseDigit = oneOf "0123456789"

joinNumber :: [Char] -> Integer
joinNumber = toInteger . foldl addThem 0
  where addThem acc d = acc * 10 + digitToInt d

parseSign :: Parser Bool
parseSign = try (char '-' >> return True) <|> return False

base10Integer :: Parser Integer
base10Integer = do
  isNegative <- parseSign
  digits <- some parseDigit
  let num = joinNumber digits
  return (if isNegative then (- num) else num) 

testDigits :: IO ()
testDigits = do
  pNL "parseDigit 123"
  print $ parseString parseDigit mempty "123"
  pNL "parseDigit abc"
  print $ parseString parseDigit mempty "abc"
  pNL "base10Integer 123abc"
  print $ parseString base10Integer mempty "123abc"
  pNL "base10Integer abc"
  print $ parseString base10Integer mempty "abc"
  pNL "base10Integer -123"
  print $ parseString base10Integer mempty "-123"
  pNL "base10Integer -123abc"
  print $ parseString base10Integer mempty "-123abc"


-- Exercises: 4
type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)

parseArea :: Parser NumberingPlanArea
parseArea = do
  skipMany (oneOf "(")
  area1 <- parseDigit
  area2 <- parseDigit
  area3 <- parseDigit
  skipMany (oneOf ")")
  let area = digitToInt area1 * 100 + digitToInt area2 * 10 + digitToInt area3
  return area

parseExchange :: Parser Exchange
parseExchange = do
  ex1 <- parseDigit
  ex2 <- parseDigit
  ex3 <- parseDigit
  let ex = digitToInt ex1 * 100 + digitToInt ex2 * 10 + digitToInt ex3
  return ex

parseLine :: Parser LineNumber
parseLine = do
  line1 <- parseDigit
  line2 <- parseDigit
  line3 <- parseDigit
  line4 <- parseDigit
  let line = digitToInt line1 * 1000 +
             digitToInt line2 * 100 +
             digitToInt line3 * 10 +
             digitToInt line4
  return line

parseCountryCode :: Parser Char
parseCountryCode = do
  country <- oneOf "1"
  oneOf "-"
  return country
  
parsePhone :: Parser PhoneNumber
parsePhone = do
  skipMany $ try parseCountryCode
  area <- parseArea
  skipMany (oneOf "- ")
  ex <- parseExchange
  skipMany (oneOf "- ")
  line <- parseLine
  return $ PhoneNumber area ex line

testPhone :: IO ()
testPhone = do
  pNL "parsePhone 123-456-7890"
  print $ parseString parsePhone mempty "123-456-7890"
  pNL "parsePhone 1234567890"
  print $ parseString parsePhone mempty "1234567890"
  pNL "parsePhone (123) 456-7890"
  print $ parseString parsePhone mempty "(123) 456-7890"
  pNL "parsePhone 1-123-456-7890"
  print $ parseString parsePhone mempty "123-456-7890"

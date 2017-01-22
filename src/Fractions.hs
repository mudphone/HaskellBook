module Text.Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

virtuousFraction :: Parser Rational
virtuousFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

type FracOrInt = Either Rational Integer

parseFracOrInt :: Parser FracOrInt
parseFracOrInt = (Left <$> try virtuousFraction) <|> (Right <$> try integer)

parseDec :: Parser Double
parseDec = do
  whole <- decimal
  char '.'
  dec <- decimal
  return (read $ show whole ++ "." ++ show dec :: Double)

type DecOrFrac = Either Double Rational

parseDecOrFrac :: Parser DecOrFrac
parseDecOrFrac = (Left <$> try parseDec) <|> (Right <$> try virtuousFraction)

main :: IO ()
main = do
  -- print $ parseString parseFraction mempty badFraction
  -- print $ parseString parseFraction mempty shouldWork
  -- print $ parseString parseFraction mempty shouldAlsoWork
  -- print $ parseString parseFraction mempty alsoBad
  print $ parseString virtuousFraction mempty badFraction
  print $ parseString virtuousFraction mempty shouldWork
  print $ parseString virtuousFraction mempty shouldAlsoWork
  print $ parseString virtuousFraction mempty alsoBad
  print $ parseString parseFracOrInt mempty "1/2"
  print $ parseString parseFracOrInt mempty "42"
  print $ parseString parseDecOrFrac mempty "1.5"
  print $ parseString parseDecOrFrac mempty "5/2"

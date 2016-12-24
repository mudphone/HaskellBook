module MainTest where

import Main
import Test.Hspec
import Test.QuickCheck

-- data Puzzle = Puzzle String [Maybe Char] [Char]
puzzleGen :: Gen Puzzle
puzzleGen = do
  a <- listOf (elements ['a'..'z'])
  return $ freshPuzzle a

instance Arbitrary Puzzle
  where arbitrary = puzzleGen

prop_fillInCharacter :: Puzzle -> Char -> Bool
prop_fillInCharacter p@(Puzzle word filledInSoFar guessed) c
  | elem c word = (elem (Just c) newFilled) && (elem c newGuessed)
  | otherwise   = (newFilled == nothings) && (newGuessed == [c])
  where (Puzzle _ newFilled newGuessed) = (fillInCharacter p c)
        nothings = map (\_ -> Nothing) word

prop_handleGuess :: Puzzle -> Char -> Bool
prop_handleGuess p@(Puzzle word filledInSoFar guessed) c
  | elem c word = (elem (Just c) newFilled) && (elem c newGuessed)
  | otherwise   = (newFilled == nothings) && (newGuessed == [c])
  where (Puzzle _ newFilled newGuessed) = (fillInCharacter p c)
        nothings = map (\_ -> Nothing) word
 
runTests :: IO ()
runTests = hspec $ do
  it "half x is always equal to 0.5 * x" $ do
    property $ \x -> x / 2 == (x :: Float) * 0.5
  it "fillInCharacter works for first char guess" $ do
    property $ \p x -> prop_fillInCharacter (p :: Puzzle) (x :: Char)
  it "handleGuess works for first char guess" $ do
    property $ \p x -> prop_handleGuess (p :: Puzzle) (x :: Char)

puzzleHelp :: Puzzle -> String
puzzleHelp (Puzzle word filled guessed) =
  "Word: " ++ word ++ " Filled: " ++ show filled ++ " Guessed: " ++ show guessed

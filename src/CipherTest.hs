module CipherTest where

import Cipher
import Test.Hspec
import Test.QuickCheck

genSafeChar :: Gen Char
genSafeChar = elements $ ['a'..'z'] ++ ['A'..'Z']

genSafeString :: Gen String
genSafeString = listOf genSafeChar

genTuple :: Gen (String, String)
genTuple = do
  a <- listOf genSafeChar
  b <- listOf genSafeChar
  return (a, b)

main :: IO ()
main = hspec $ do
  it "unCaesar after caesar should be the identity" $ do
    forAll genSafeString $ \word -> length word > 0 ==>
      (unCaesar $ caesar word) == word
  it "unVigenere after vigenere should be the identity" $ do
    forAll genTuple $ \(word, pass) -> length word > 0 && length pass > 0 ==>
      (unVigenere (vigenere word pass) pass) == word 
      


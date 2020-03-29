module Main
  ( main
  ) where

import Base
import Hard
import Parser
import Simple
import Test.Hspec (describe, hspec, it, shouldBe)
import Test.QuickCheck (property)

stringByList :: (a -> String) -> [a] -> String
stringByList showFunc l =
  if len > 0
    then (show $ len) ++ "," ++ helper l
    else ""
  where
    len = length l
    helper (x : []) = showFunc x
    helper (x : xs) = showFunc x ++ ", " ++ helper xs
    helper _        = error "Cannot be"

stringByListList :: Show a => [[a]] -> String
stringByListList = stringByList (stringByList show)

main :: IO ()
main = hspec $ do
  describe "Testing Base Parser combinators" $ do
    it "Testing OK combinator" $ do
      runParser ok "" `shouldBe` Just ((), "")
      runParser ok [1,2,3,4] `shouldBe` Just ((), [1,2,3,4] :: [Int])
    it "Testing OK combinator property" $ do
      property $ \x -> runParser ok x `shouldBe` Just ((), x :: [Int])
    it "Testing EOF combinator" $ do
      runParser eof "" `shouldBe` Just ((), "")
      runParser eof "1" `shouldBe` Nothing
    it "Testing EOF combinator property" $ do
      property $ \x -> runParser eof x `shouldBe`
        case x :: [Int] of
          [] -> Just ((), [])
          _  -> Nothing
    it "Testing satisfy combinator" $ do
      runParser (satisfy (\x -> x > 10)) [20, 30, 2] `shouldBe` Just (20 :: Int, [30, 2])
      runParser (satisfy (\x -> x `mod` 2 == 0)) [1 :: Int, 2, 3] `shouldBe` Nothing
    it "Testing element combinator" $ do
      runParser (element 1) [1, 2, 3] `shouldBe` Just (1 :: Int, [2, 3])
      runParser (element '(') ")(())" `shouldBe` Nothing
    it "Testing element combinator property" $ do
      property $ \d x xs -> runParser (element d) (x:xs) `shouldBe`
        if x == d
        then Just (x :: Char, xs)
        else Nothing
    it "Testing stream combinator" $ do
      runParser (stream "123") "123415" `shouldBe` Just ("123", "415")
      runParser (stream "1234") "1" `shouldBe` Nothing
  describe "Testing simple task parsers" $ do
    it "Testing brackets parser" $ do
      runParser parseBrackets "(())" `shouldBe` Just ((), "")
      runParser parseBrackets ")(" `shouldBe` Nothing
    it "Testing integer parser" $ do
      runParser parseInteger "-123" `shouldBe` Just (-123, "")
      runParser parseInteger "-123asd" `shouldBe` Just (-123, "asd")
      -- strange but it's used in list parser in hard task
      runParser parseInteger "-asd" `shouldBe` Just (0, "asd")
      runParser parseIntegerOnly "+12" `shouldBe` Just (12, "")
      runParser parseIntegerOnly "+1a2" `shouldBe` Nothing
      runParser parseIntegerOnly "mvdb" `shouldBe` Nothing
    it "Testing integer parser property" $ do
      property $ \x -> runParser parseIntegerOnly (show x) `shouldBe` Just (x, "")
  describe "Testing hard task parser" $ do
    it "Testing list whitespace skipper parser" $ do
      runParser omitWS "   123" `shouldBe` Just ((), "123")
      runParser omitWS "   " `shouldBe` Just ((), "")
      runParser omitWS "" `shouldBe` Just ((), "")
    it "Testing list integer parser" $ do
      runParser listParser "2, 1,   3" `shouldBe` Just ([1, 3], "")
      runParser listParser "2, abs, d" `shouldBe` Nothing
      runParser listParser "" `shouldBe` Just ([], "")
    it "Testing list list integer parser" $ do
      runParser listlistParser "2, 2, 1, 4, 1, -100" `shouldBe` Just ([[1, 4], [-100]], "")
      runParser listlistParser "afnsgsd1241k" `shouldBe` Nothing
      runParser listlistParser "" `shouldBe` Just ([], "")
    it "Testing list integer parser property" $ do
      property $ \x -> runParser listParser (stringByList (show) (x :: [Int])) `shouldBe` Just (x, [])
    it "Testing list list integer parser property" $ do
      property $ \x -> runParser listlistParser (stringByListList (x :: [[Int]])) `shouldBe` Just (x, [])

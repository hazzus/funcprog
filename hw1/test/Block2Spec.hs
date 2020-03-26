module Block2Spec
    ( spec
    ) where

import Block1
import Block2
import Data.List (sort)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (property)



spec :: Spec
spec = do
  describe "Testing property Foldable of Tree" $ do
    it "fromList . toList is equivalent to sort for [Int]" $ do
      property $ \l -> (toList (fromList (l :: [Int]))) `shouldBe` (sort l)
    it "fromList . toList is equivalent to sort for String" $ do
      property $ \l -> (toList (fromList (l :: String))) `shouldBe` (sort l)
  describe "Testing property of split and join functions" $ do
    it "Testing property joinWith x . splitOn x is equiv to id" $ do
      property $ \l d -> (joinWith d (splitOn d l)) `shouldBe` (l :: [Int])
    it "Testing same with other type String" $ do
      property $ \l d -> (joinWith d (splitOn d l)) `shouldBe` (l :: String)
    it "Testing same with other type [[Int]]" $ do
      property $ \l d -> (joinWith d (splitOn d l)) `shouldBe` (l :: [[Int]])


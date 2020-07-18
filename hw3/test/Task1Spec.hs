{-# OPTIONS_GHC -fno-warn-orphans #-}
module Task1Spec
  ( spec
  ) where

import           Task1
import qualified Task1Naive      as Naive

import           Test.Hspec      (Spec, describe, it, shouldBe, shouldSatisfy)
import           Test.QuickCheck (Arbitrary, arbitrary, property)

a :: Point
a = Point 2 2
b :: Point
b = Point 2 (-2)
c :: Point
c = Point (-2) (-2)
d :: Point
d = Point (-2) 2
square :: [Point]
square = [a, b, c, d]
ptriag :: [Point]
ptriag = [Point 0 4, Point 3 0, Point 0 0]

instance Arbitrary Point where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Point x y

spec :: Spec
spec = do
  describe "Testing naive versions" $ do
    it "Testing plus" $ do
      Naive.plus a c `shouldBe` Point 0 0
      Naive.plus a a `shouldBe` Point 4 4
      Naive.plus c d `shouldBe` Point (-4) 0
    it "Testing minus" $ do
      Naive.minus a a `shouldBe` Point 0 0
      Naive.minus b c `shouldBe` Point 4 0
    it "Testing scalar product" $ do
      Naive.scalarProduct a b `shouldBe` 0
      Naive.scalarProduct b d `shouldBe` (-8)
    it "Testing cross product" $ do
      Naive.crossProduct a c `shouldBe` 0
      Naive.crossProduct a d `shouldBe` 8
    it "Testing perimeter" $ do
      Naive.perimeter square `shouldBe` 16
      Naive.perimeter ptriag `shouldBe` 12.0
    it "Testing double area" $ do
      Naive.doubleArea square `shouldBe` 32
      Naive.doubleArea ptriag `shouldBe` 12
  describe "Property testing strict versions" $ do
    it "Testing plus" $ do
      property $ \x y ->
        plus x y `shouldBe` Naive.plus x y
    it "Testing minus" $ do
      property $ \x y ->
        minus x y `shouldBe` Naive.minus x y
    it "Testing scalar product" $ do
      property $ \x y ->
        scalarProduct x y `shouldBe` Naive.scalarProduct x y
    it "Testing cross product" $ do
      property $ \x y ->
        crossProduct x y `shouldBe` Naive.crossProduct x y
    it "Testing perimeter" $ do
      property $ \x ->
        perimeter x - Naive.perimeter x `shouldSatisfy` (< 1e-10) . abs
    it "Testin double area" $ do
      property $ \x ->
        doubleArea x `shouldBe` Naive.doubleArea x

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Block1Spec
  ( spec
  ) where

import Block1
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (property)

spec :: Spec
spec = do
  describe "Testing properties of Num" $ do
    it "Testing add" $ do
      property $ \a b ->
        if a >= 0 && b >= 0
        then (fromInteger a) + (fromInteger b :: Nat) `shouldBe` fromInteger (a + b)
        else True `shouldBe` True
    it "Testing sub" $ do
      property $ \a b ->
        if a >= b && b >= 0
        then (fromInteger a) - (fromInteger b :: Nat) `shouldBe` fromInteger (a - b)
        else True `shouldBe` True
  describe "Testing properties of Tree" $ do
    it "Testing insert + remove doesn't change the size" $ do
      property $ \l a -> size (remove (insert (fromList (l :: [Int])) a) a) `shouldBe` length l

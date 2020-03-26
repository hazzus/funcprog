module Block4Spec
  ( spec
  ) where

import Block4
import Data.List (intercalate)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (property)

spec :: Spec
spec = do
  describe "Testing task 1" $ do
    it "Testing stringSum function" $ do
      (stringSum "1 2 3") `shouldBe` (Just 6)
      (stringSum "1 2 3 a") `shouldBe` Nothing
    it "Testing strinSum property" $ do
      property $ \l -> (stringSum $ intercalate " " $ map show l) `shouldBe` (Just (sum l))


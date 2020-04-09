module Block5Spec
  ( spec
  ) where

import Block5
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Testing Task 1" $ do
    it "Testing eval function" $ do
      eval (Sum (Const 2) (Const 3)) `shouldBe` Right 5
      eval (Mul (Const 10) (Const 20)) `shouldBe` Right 200
      eval (Div (Const 10) (Const 3)) `shouldBe` Right 3
      eval (Sum (Mul (Const 3) (Const (-2))) (Sub (Const 3) (Const 5))) `shouldBe` Right (-8)
      eval (Pow (Const 3) (Const 4)) `shouldBe` Right 81
      eval (Div (Const 1) (Const 0)) `shouldBe` Left (ArithmeticError { message = "Division by zero" })
      eval (Pow (Const 1) (Const (-2))) `shouldBe` Left (ArithmeticError { message = "Negative power" })
      eval (Mul (Pow (Const 4) $ Const (-1)) $ Div (Const 1) (Const 0)) `shouldBe` Left (ArithmeticError { message = "Negative power" })

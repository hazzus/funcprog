module Block6Spec
  ( spec
  ) where

import Block6
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "Testing Block 6" $ do
    it "Testing Task 1" $ do
      True `shouldBe` True -- TODO
    it "Testing Task 2" $ do
      runParser ok [1::Int] `shouldBe` Just ((), [1::Int])
      runParser ok "aaa" `shouldBe` Just ((), "aaa")
      runParser eof "" `shouldBe` Just ((), [])
      runParser eof "a" `shouldBe` Nothing
      runParser (element '(') "((aa" `shouldBe` Just ('(', "(aa")
      runParser (stream "aaa") "aaaabbb" `shouldBe` Just ("aaa", "abbb")
      runParser (satisfy id) [True, False, True] `shouldBe` Just (True, [False, True])
      runParser (satisfy id) [False, False, True] `shouldBe` Nothing
    it "Testing Task3" $ do
      runParser parseBrackets "(())" `shouldBe` Just ((), "")
      runParser parseBrackets ")(" `shouldBe` Nothing

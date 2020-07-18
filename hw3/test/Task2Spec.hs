{-# OPTIONS_GHC -fno-warn-orphans #-}
module Task2Spec
  ( spec
  ) where

import           Task5
import           Task6
import           Task7

import           Lens.Micro (each, (^.), (^..), (^?!))


import           Test.Hspec (Spec, describe, it, shouldBe)

fs :: FS
fs = Dir { _name = "src"
         , _contents =
            [ File {_name = "one.txt"}
            , File {_name = "two.hs"}
            ]
         }


spec :: Spec
spec = do
  describe "Testing basic lens" $
    it "Testing name lens" $
      fs ^. name `shouldBe` "src"

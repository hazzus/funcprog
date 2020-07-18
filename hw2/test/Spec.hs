module Main
  ( main
  ) where

import           Control.Monad.Trans.State (evalStateT, execStateT)
import qualified Data.Map                  as Map (fromList, singleton, (!))
import           Test.Hspec                (describe, hspec, it, shouldBe)

import           CommandParser             (wordsCorrect)
import           FileSystem

testFS :: FileEntity
testFS = (emptyDirectory "/" "home")
  { dirContent = Map.fromList
    [ ("dir1", emptyDirectory "/home" "dir1")
    , ("file1", emptyFile "/home" "file1")
    ]
  }

main :: IO ()
main = hspec $ do
  describe "Testing some utilities" $ do
    it "Testing wordsCorrect function" $ do
      wordsCorrect "hello" `shouldBe` Right ["hello"]
      wordsCorrect "echoto /lol/kek    \"hello, friend!\"" `shouldBe` Right ["echoto", "/lol/kek", "hello, friend!"]
      wordsCorrect "unmatched \" quote" `shouldBe` Left "Unmatched quote"
      wordsCorrect "many \" of \"   unmat \"ched quotes\"\"" `shouldBe` Left "Unmatched quote"
  describe "Testing the fs" $ do
    it "Testing listing functions" $ do
      evalStateT (listCurrentDirectory) testFS `shouldBe` Right [".", "dir1", "file1"]
      evalStateT (listSpecDirectory "dir1") testFS `shouldBe` Right ["."]
      evalStateT (listSpecDirectory "nosuch") testFS `shouldBe` Left (NoSuchPath "nosuch")
      evalStateT (listSpecDirectory "file1") testFS `shouldBe` Left (NotDirectory "file1")
    it "Testing getting curdir" $ do
      getCurDir testFS `shouldBe` "/home"
      getCurDir (dirContent testFS Map.! "dir1") `shouldBe` "/home/dir1"
    it "Testing changing directory" $ do
      execStateT (changeDirectory "dir1") testFS `shouldBe` Right ((emptyDirectory "/home" "dir1") { dirContent = Map.singleton ".." testFS })
      execStateT (changeDirectory "dir666") testFS `shouldBe` Left (NoSuchPath "dir666")
    it "Testing catFile" $ do
      evalStateT (catFile "file1") testFS `shouldBe` Right ""
      evalStateT (catFile "file666") testFS `shouldBe` Left (NoSuchPath "file666")
      evalStateT (catFile "dir1") testFS `shouldBe` Left (NotFile "dir1")

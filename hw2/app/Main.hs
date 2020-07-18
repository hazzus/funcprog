module Main where

import           Control.Monad.State (execStateT)
import           System.Directory    (makeAbsolute)

import           FileSystem          (download, upload)
import           MainArgsParser
import           Pipeline            (interpretor)

main :: IO ()
main = do
  dir <- execArgParserInfo >>= makeAbsolute
  root <- upload dir
  newRoot <- execStateT interpretor root
  download newRoot dir

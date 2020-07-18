module Util
  ( catchingExecution
  ) where

import           Control.Monad.Trans.State (State, get, put, runStateT)

import           FileSystem                (FileEntity, FileSystem)


-- | Performs a cathing of 'FileSystem.FileSystemError', doesn't change the whole programm state if something fails,
-- so is providing atomicity
catchingExecution :: FileSystem a -> (a -> String) -> State FileEntity String
catchingExecution st f = do
  fe <- get
  let res = runStateT st fe
  case res of
    Left e -> return $ "Error occured: " ++ show e
    Right (x, newfe) -> do
      put newfe
      return $ f x

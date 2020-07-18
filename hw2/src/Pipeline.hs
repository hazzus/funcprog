module Pipeline
  ( interpretor
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State    (State, get, put, runState)
import           Data.List              (intercalate)
import           Options.Applicative    (ParserResult (..), execFailure)
import           System.IO              (hFlush, isEOF, stdout)

import           Command
import           CommandParser
import           FileSystem
import           HitPipeline
import           Util

-- | Basic File System Environment that endlessly loops waiting for quit command
-- and performs the commands given
interpretor :: FileSystemEnv ()
interpretor = do
  fs <- get
  raw <- liftIO $ readCommand fs
  if raw == "quit"
    then return ()
    else (parser raw >>= executor >>= printer >> interpretor)

readCommand :: FileEntity -> IO String
readCommand root = do
  putStr $ getCurDir root ++ " $ "
  hFlush stdout
  isEOF >>= \x -> if x then return "quit" else getLine

parser :: String -> FileSystemEnv Command
parser raw = do
  let splitted = wordsCorrect raw
  case splitted of
    Left e -> return $ Fail e
    Right spl -> do
      case parseCommand spl of
        Success a -> return a
        Failure f -> do
          let (h, _, _) = execFailure f ""
          return $ Fail $ show h
        _ -> return $ Fail "Completion is not supported and not supposed to be used here"

executor :: Command -> FileSystemEnv String
executor cmd = do
  fs <- get
  let (res, nfs) = runState (executeCommand cmd) fs
  put nfs
  return res

printer :: String -> FileSystemEnv ()
printer val =
  case val of
    [] -> return ()
    _  -> liftIO $ putStrLn val

findResultFormat :: [FilePath] -> String
findResultFormat [] = "Nothing found"
findResultFormat re = "Found:\n" ++ intercalate "\n" re

executeCommand :: Command -> State FileEntity String
executeCommand (Fail mes)   = return mes
executeCommand (Cd dir)     = catchingExecution (changeDirectory dir) $ const ""
executeCommand (Dir a)      = catchingExecution (listCurrentDirectory) $ intercalate "\n" . filter (\x -> a || head x /= '.')
executeCommand (Ls dir a)   = catchingExecution (listSpecDirectory dir) $ intercalate "\n" . filter (\x -> a || head x /= '.')
executeCommand (Cat f)      = catchingExecution (catFile f) id
executeCommand (Rm fs r)    = catchingExecution (removePaths fs r) $ const ""
executeCommand (Info p)     = catchingExecution (getInfo p) show
executeCommand (MkFile fs)  = catchingExecution (createFiles fs) $ const ""
executeCommand (MkDir ds p) = catchingExecution (createDirectories ds p) $ const ""
executeCommand (EchoTo f t) = catchingExecution (echoToFile f t) $ const ""
executeCommand (Find n w)   = catchingExecution (findFileByName w n) findResultFormat
executeCommand (Hit cmd)    = executeHit cmd

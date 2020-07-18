module HitPipeline
  ( executeHit
  ) where

import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State (State, get, put)
import           Data.ByteString.Char8     (length, pack)
import           Data.List                 (intercalate)
import qualified Data.Map                  as Map
import           Prelude                   hiding (length)
import           System.FilePath           ((</>))

import           Command                   (HitCommand (..))
import           FileSystem                (FileEntity (..), FileSystem,
                                            FileSystemError (HitFailed),
                                            HitError (..), catFile,
                                            fsCreateDirectory, removePath,
                                            withDirectory)
import           Information
import           Util

-- | Function that executes a given hit command
executeHit :: HitCommand -> State FileEntity String
executeHit (Init)       = catchingExecution hitInit $ const ""
executeHit (Update com) = catchingExecution (hitUpdate com) ("Created revision number: " ++)
executeHit (Log)        = catchingExecution hitLog $ ("VCS revisions log:\n" ++) . intercalate "\n"
executeHit (Restore r)  = catchingExecution (hitRestore r) $ const $ "Restored revision " ++ show r
executeHit (Deinit)     = catchingExecution hitDeinit $ const ""
executeHit (HitCat f r) = catchingExecution (hitCat f r) id

executeHit (Add _)      = return "Unsupported command"

isInitialized :: FileEntity -> Bool
isInitialized fe =
  case Map.lookup ".hifm" (dirContent fe) of
    Nothing -> False
    _       -> True

getInternal :: FileSystem FileEntity
getInternal = do
  fe <- get
  case Map.lookup ".hifm" (dirContent fe) of
    Just d@(Directory _ _) -> return d
    Nothing                -> lift $ Left $ HitFailed NotInitialized
    _                      -> lift $ Left $ HitFailed InvalidHitState

hitInit :: FileSystem ()
hitInit = do
  fe <- get
  if not $ isInitialized fe
    then fsCreateDirectory ".hifm" False
    else lift $ Left $ HitFailed AlreadyInitialized

hitDeinit :: FileSystem ()
hitDeinit = getInternal >> removePath ".hifm" True

hitUpdate :: String -> FileSystem String
hitUpdate cmnt = do
  cur <- get
  fe <- getInternal
  let revNum = show $ Map.size (dirContent fe)
  let revDir = ".hifm" </> revNum
  fsCreateDirectory revDir False
  withDirectory revDir $ do
    hifmFE <- get
    let packedComment = pack cmnt
    let comLen = toInteger $ length packedComment
    let newFile = File (commentInfo (info hifmFE) comLen) packedComment
    let commentOnly = Map.singleton "comment" newFile
    let commentAndSnap = Map.insert (name $ info cur) (updateInfo cur $ info hifmFE) commentOnly
    put $ Directory (info hifmFE) commentAndSnap
    return revNum
  where
    commentInfo parent sz =
      FileInfo
        { path = path parent </> "comment"
        , permissions = permissions parent
        , size = sz
        , fileType = ""
        }
    updateInfo cur parent = cur { info = (info cur) { path = path parent </> (name $ info cur) } }

hitLog :: FileSystem [String]
hitLog = do
  fe <- getInternal
  withDirectory ".hifm" $ Map.elems <$> mapM prettyRevision (dirContent fe)
  where
    prettyRevision :: FileEntity -> FileSystem String
    prettyRevision e = do
      let revDir = name $ info e
      catFile (revDir </> "comment") >>= \cmnt ->
        return $ "Revision " ++ revDir ++ ": " ++ cmnt

hitRestore :: String -> FileSystem ()
hitRestore rev = do
  internal <- getInternal
  cur <- get
  let internalCont = dirContent internal
  case Map.lookup rev internalCont of
    Nothing -> lift $ Left $ HitFailed NoSuchRevision
    Just (Directory _ revCont) ->
      case Map.lookup (name $ info cur) revCont of
        Nothing -> invalidState
        Just (Directory _ oldCont) -> do
          let curContent = dirContent cur
          let newCont = Map.insert ".hifm" (curContent Map.! ".hifm") oldCont
          case Map.lookup ".." (dirContent cur) of
            Nothing -> put $ Directory (info cur) newCont
            Just newParent -> put $ Directory (info cur) $ Map.insert ".." newParent newCont
          return ()
        _ -> invalidState
    _ -> invalidState
  where
    invalidState = lift $ Left $ HitFailed InvalidHitState

hitCat :: FilePath -> String -> FileSystem String
hitCat f r = do
  fe <- getInternal
  case Map.lookup r (dirContent fe) of
    Nothing -> lift $ Left $ HitFailed NoSuchRevision
    _ -> get >>= \x -> withDirectory (".hifm" </> r </> (name $ info x)) $ catFile f

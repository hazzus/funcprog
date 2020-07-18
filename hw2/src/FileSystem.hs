module FileSystem
  ( FileEntity (..)
  , FileSystemEnv
  , FileSystem
  , FileSystemError (..)
  , HitError (..)
  , upload
  , download
  , getCurDir
  , changeDirectory
  , withDirectory
  , listCurrentDirectory
  , listSpecDirectory
  , catFile
  , removePath
  , removePaths
  , getInfo
  , createFile
  , createFiles
  , fsCreateDirectory
  , createDirectories
  , echoToFile
  , findFileByName
  -- for testing export
  , emptyFile
  , emptyDirectory
  ) where

import           Control.DeepSeq                   (($!!))
import           Control.Exception                 (catch, throw, try)
import           Control.Monad.Extra               (concatForM)
import           Control.Monad.Trans.Class         (lift)
import           Control.Monad.Trans.State         (StateT, get, gets, put,
                                                    runStateT)
import           Data.ByteString.Char8             (ByteString, append, empty,
                                                    length, pack, readFile,
                                                    unpack, writeFile)
import           Data.List                         (sort)
import qualified Data.Map                          as Map
import           Data.Time.Clock                   (getCurrentTime)
import           Prelude                           hiding (length, readFile,
                                                    writeFile)
import           System.Directory                  (createDirectory,
                                                    doesDirectoryExist,
                                                    emptyPermissions,
                                                    listDirectory, readable,
                                                    removePathForcibly,
                                                    renameDirectory, searchable,
                                                    setPermissions,
                                                    withCurrentDirectory,
                                                    writable)
import           System.Directory.Internal.Prelude (isPermissionError)
import           System.FilePath                   (splitDirectories,
                                                    takeDirectory,
                                                    takeExtension, takeFileName,
                                                    (</>))

import           Information

-- | Basic representative of the filesystem tree
-- node can rather be file or directory
-- info acquires Information about file (sometimes it's caculated
-- lazily, so be carefull to use it), dirContent returns all FileEntities
-- inside directory, (note that they should be unique, file and dir with same
-- name lead to undefined behaviour
data FileEntity
  -- | Directory constructor
  = Directory { info       :: Information -- ^ information of File Entity
              , dirContent :: Map.Map String FileEntity -- ^ directory content
              }
  -- | File constructor
  | File { info        :: Information
         , fileContent :: ByteString -- ^ file content
         }
  deriving (Show, Eq)

-- | Basic "exception" for 'FileSystem' methods (always returned
-- as Left of 'Data.Either'
data FileSystemError
  = NotDirectory FilePath
  | NoSuchPath FilePath
  | InvalidState String
  | NotFile FilePath
  | PermissionDenied FilePath
  | AlreadyExists FilePath
  | HitFailed HitError
  deriving (Eq, Show)

-- | Basic "exception" for hit(VCS) commands
-- can only be used as part of 'FileSystemError'
data HitError
  = AlreadyInitialized
  | NotInitialized
  | InvalidHitState
  | NoSuchRevision
  deriving (Eq, Show)

-- | File System Environment, allows to operate the input-output cycle
-- for the command line interface
type FileSystemEnv = StateT FileEntity IO

-- | Quite pure state to operate with File System. Actually is a StateT
-- of 'FileEntity' with 'Either' inside for handling errors
type FileSystem = StateT FileEntity (Either FileSystemError)


-- functions for IO

readFile' :: FilePath -> IO ByteString
readFile' p = do
  f <- readFile p
  return $!! f

-- | IO function to upload a real directory into the virtual file system
upload :: FilePath -> IO FileEntity
upload p = do
  isDir <- doesDirectoryExist p
  if isDir
    then do
      content <- catch (listDirectory p) $ catchPrint []
      entities <- catch (withCurrentDirectory p $ mapM upload content) $ catchPrint []
      Directory <$> fromFilePath p <*> (return $ Map.fromList $ zip content entities)
    else do
      fileCont <- catch (readFile' p) $ catchPrint empty
      File <$> fromFilePath p <*> (return fileCont)
  where
    catchPrint :: a -> IOError -> IO a
    catchPrint rval e =
      if isPermissionError e
        then return rval
        else throw e

cdRoot :: FileSystem ()
cdRoot = do
  fe <- get
  case Map.lookup ".." (dirContent fe) of
    Nothing -> return ()
    _ -> do
      changeDirectory ".."
      cdRoot

-- | Downloads virtual file system to the disk. Moves original to the another folder
-- called "name"-old"time"
download :: FileEntity -> FilePath -> IO ()
download e p = do
  let rt = runStateT cdRoot e
  case rt of
    Left ex -> do
      putStrLn $ "Virtual cd to root failed: " ++ show ex
      putStrLn "Panic: changes stayed unperformed"
    Right (_, ne)  -> do
      t <- getCurrentTime
      let tmp = p ++ show t
      createDirectory tmp
      setPermissions tmp $ permissions $ info ne
      load <- try (withCurrentDirectory tmp $ mapM_ downloadImpl $ Map.elems (dirContent ne))
      case load of
        Left ex -> panic ex >> removePathForcibly tmp
        Right _ -> do
          renameDirectory p $ p ++ "-old" ++ show t
          renameDirectory tmp p
  where
    downloadImpl :: FileEntity -> IO ()
    downloadImpl ent =
      case ent of
        File i c -> do
          writeFile (name i) c
          setPermissions (name i) (permissions i)
        Directory i c -> do
          createDirectory (name i)
          setPermissions (name i) (permissions i)
          -- putStrLn $ (name i) ++ show (permissions i)
          withCurrentDirectory (name i) $ mapM_ downloadImpl $ Map.elems c

    panic :: IOError -> IO ()
    panic ex = do
      putStrLn $ show ex
      putStrLn "Panic: changes styed unperformed"

-- | Show the path of current directory
getCurDir :: FileEntity -> FilePath
getCurDir = path . info


-- | Constructs an empty file with basic permissions basing on name and parents info
emptyFile :: FilePath -> FilePath -> FileEntity
emptyFile dirPath fileName =
  File { info = FileInfo { path = dirPath </> fileName
                         , permissions = emptyPermissions { readable = True
                                                          , writable = True
                                                          , searchable = True
                                                          }
                         , size = 0
                         , fileType = takeExtension fileName
                         }
       , fileContent = empty
       }

-- | Constructs an empty directory with no content basing on name and parents info
emptyDirectory :: FilePath -> FilePath -> FileEntity
emptyDirectory dirPath dirName =
  Directory { info = DirInfo { path = dirPath </> dirName
                             , permissions = emptyPermissions { readable = True
                                                              , writable = True
                                                              , searchable = True
                                                              }
                             , size = 0
                             , fileCount = 0
                             }
            , dirContent = Map.empty
            }

-- Commands states

-- | 'changeDirectory' dir changes current directory to the dir
changeDirectory :: FilePath -> FileSystem ()
changeDirectory = changeDirectorySplitted . splitDirectories
  where
    changeDirectorySplitted :: [FilePath] -> FileSystem ()
    changeDirectorySplitted [] = return ()
    changeDirectorySplitted (actual : other) = do
      fe <- get
      if actual == "."
      then changeDirectorySplitted other
      else case fe of
        File _ _ -> lift $ Left $ NotDirectory actual
        d@(Directory _ cont) ->
          case Map.lookup actual cont of
            Nothing              -> lift $ Left $ NoSuchPath actual
            Just (File _ _ )     -> lift $ Left $ NotDirectory actual
            Just (Directory i c) ->
              if readable (permissions i)
              then do
                if actual == ".."
                then do
                  let curdir = name $ info d
                  let nd = Directory (info d) $ Map.delete ".." cont
                  put $ Directory i $ Map.insert curdir nd c
                else put $ Directory i $ Map.insert ".." d c
                changeDirectorySplitted other
              else lift $ Left $ PermissionDenied actual

-- | 'withDirectory' dir action performs action inside dir
withDirectory :: FilePath -> FileSystem a -> FileSystem a
withDirectory = withDirectorySplitted . splitDirectories
  where
    withDirectorySplitted :: [FilePath] -> FileSystem a -> FileSystem a
    withDirectorySplitted [] action = action
    withDirectorySplitted (actual : other) action = do
      fe <- get
      if actual == "."
      then withDirectorySplitted other action
      else case fe of
        File _ _ -> lift $ Left $ NotDirectory actual
        Directory i cont ->
          if readable (permissions i)
          then case Map.lookup actual cont of
            Nothing -> lift $ Left $ NoSuchPath actual
            Just (File _ _) -> lift $ Left $ NotDirectory actual
            Just d -> do
              -- TODO think of something like bind without changing state
              let res = runStateT (withDirectorySplitted other action) d
              case res of
                Left e -> lift $ Left e
                Right (ret, nfe) -> do
                  put $ Directory i $ Map.insert actual nfe cont
                  return ret
          else lift $ Left $ PermissionDenied actual

-- | 'listCurrentDirectory' lists all files and directories inside the current directory
listCurrentDirectory :: FileSystem [FilePath]
listCurrentDirectory = gets $ sort . ("." :) . Map.keys . dirContent

-- | 'listSpecDirectory' dir lists all files and directories inside dir
listSpecDirectory :: FilePath -> FileSystem [FilePath]
listSpecDirectory p = withDirectory p listCurrentDirectory

-- | 'fsCreateDirectory' dir parents creates dir from the current directory. If parents
-- is True, then creates all parents if needed
fsCreateDirectory :: FilePath -> Bool -> FileSystem ()
fsCreateDirectory d p =
  if not p
  then withDirectory (takeDirectory d) (createDirectoryImpl (takeFileName d))
  else createDirectoriesSplitted (splitDirectories d)
  where
    createDirectoryImpl :: FilePath -> FileSystem ()
    createDirectoryImpl fileName = do
      fe <- get
      case Map.lookup fileName (dirContent fe) of
        Nothing -> do
          put $ Directory (info fe) (Map.insert fileName (emptyDirectory (path $ info fe) fileName) (dirContent fe))
        Just (File _ _) -> lift $ Left $ NotDirectory fileName
        Just (Directory _ _) -> return () -- lift $ Left $ AlreadyExists fileName

    createDirectoriesSplitted :: [FilePath] -> FileSystem ()
    createDirectoriesSplitted [] = return ()
    createDirectoriesSplitted (actual : other) =
      createDirectoryImpl actual >> withDirectory actual (createDirectoriesSplitted other)

-- | 'createDirectories' dirs parents does same as 'fsCreateDirectory' but for many paths
-- if parents is True also creates all parents
createDirectories :: [FilePath] -> Bool -> FileSystem ()
createDirectories ds p = mapM_ (flip fsCreateDirectory p) ds

-- | 'catFile' file outputs the content of file
catFile :: FilePath -> FileSystem String
catFile p = withDirectory (takeDirectory p) (readFileImpl $ takeFileName p)
  where
    readFileImpl :: FilePath -> FileSystem String
    readFileImpl fileName = do
      fe <- get
      case Map.lookup fileName (dirContent fe) of
        Nothing              -> lift $ Left $ NoSuchPath fileName
        Just (Directory _ _) -> lift $ Left $ NotFile fileName
        Just (File i c)      ->
          if readable (permissions i)
          then return $ unpack c
          else lift $ Left $ PermissionDenied fileName

-- | 'removePath' dir recurse removes a file, or
-- removes directory recursive if recurse is set to True
removePath :: FilePath -> Bool -> FileSystem ()
removePath p r = withDirectory (takeDirectory p) (removePathImpl $ takeFileName p)
  where
    removePathImpl :: FilePath -> FileSystem ()
    removePathImpl fileName = do
      fe <- get
      let content = dirContent fe
      case Map.lookup fileName content of
        Nothing -> lift $ Left $ NoSuchPath fileName
        Just (File i _) ->
          if writable $ permissions i
            then do
              put $ Directory (info fe) (Map.delete fileName content)
              return ()
            else lift $ Left $ PermissionDenied fileName
        Just (Directory _ _) ->
          if r
          then put $ Directory (info fe) (Map.delete fileName content)
          else lift $ Left $ NotFile fileName

-- | Same as 'removePath' but for many paths
removePaths :: [FilePath] -> Bool -> FileSystem ()
removePaths ps r = mapM_ (flip removePath r) ps

-- | Get information for directory or folder
getInfo :: FilePath -> FileSystem Information
getInfo p = withDirectory (takeDirectory p) (getInfoImpl $ takeFileName p)
  where
    getInfoImpl :: FilePath -> FileSystem Information
    getInfoImpl fileName = do
      fe <- get
      case Map.lookup fileName (dirContent fe) of
        Nothing              -> lift $ Left $ NoSuchPath fileName
        Just (File i _)      -> return i
        Just d@(Directory i c) -> do
          let newInfo = i { fileCount = toInteger $ Map.size c
                          , size = collectSizeRec d
                          }
          withDirectory fileName $ put $ Directory newInfo c
          return newInfo

    collectSizeRec :: FileEntity -> Integer
    collectSizeRec fe =
      case fe of
        File i _      -> size i
        Directory _ c -> Map.foldl' (\y x -> y + collectSizeRec x) 0 c

-- | Create and empty file at the given path
createFile :: FilePath -> FileSystem ()
createFile p = withDirectory (takeDirectory p) (createFileImpl $ takeFileName p)
  where
    createFileImpl :: FilePath -> FileSystem ()
    createFileImpl fileName = do
      fe <- get
      let content = dirContent fe
      case Map.lookup fileName (dirContent fe) of
        Just _ -> lift $ Left $ AlreadyExists fileName
        Nothing -> do
          put $ Directory (info fe) (Map.insert fileName (emptyFile (path $ info fe) fileName) content)
          return ()

-- | Same as 'createFile' but for many files
createFiles :: [FilePath] -> FileSystem ()
createFiles = mapM_ createFile

-- | 'echoToFile' file text will append text to the file
echoToFile :: FilePath -> String -> FileSystem ()
echoToFile f t = withDirectory (takeDirectory f) (writeFileImpl (takeFileName f) t)
  where
    writeFileImpl :: FilePath -> String -> FileSystem ()
    writeFileImpl fileName text = do
      fe <- get
      case Map.lookup fileName (dirContent fe) of
        Nothing -> do
          createFile fileName
          writeFileImpl fileName text
        Just (Directory _ _) -> lift $ Left $ NotFile fileName
        Just (File i x) ->
          if writable (permissions i)
            then do
              let packed = pack text
              let newFile = File i {size = size i + toInteger (length packed)} $ append x packed
              put $ Directory (info fe) (Map.insert fileName newFile (dirContent fe))
              return ()
          else lift $ Left $ PermissionDenied fileName

-- | 'findFileByName' will search for all files in specified directory recursively
-- and will show all exactly matched by name
findFileByName :: FilePath -> FilePath -> FileSystem [FilePath]
findFileByName whr = withDirectory whr . doFind whr
  where
    doFind :: FilePath -> FilePath -> FileSystem [FilePath]
    doFind prefix searchName = do
      fe <- get
      concatForM (Map.toList $ dirContent fe) $ \(n, e) ->
        case e of
          File _ _ -> return $ if n == searchName then [prefix </> searchName] else []
          _        -> withDirectory n $ doFind (prefix </> n) searchName

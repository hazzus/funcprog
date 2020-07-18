module Information
  ( Information(..)
  , fromFilePath
  , name
  ) where

import           Control.Exception (throw)
import           Data.List         (intercalate)
import           System.Directory  (Permissions, doesDirectoryExist,
                                    doesFileExist, getFileSize, getPermissions,
                                    makeAbsolute)
import           System.FilePath   (takeExtension, takeFileName)
import           System.IO.Error   (doesNotExistErrorType, mkIOError)

-- | Information data type for everything about File or Directory
data Information
  -- | Constructor for file info
  = FileInfo
    { path        :: FilePath -- ^ absolute path of entity
    , permissions :: Permissions -- ^ permissions for entity
    , size        :: Integer -- ^ size of entity in bytes
    , fileType    :: FilePath -- ^ file extension
    }
  | DirInfo
    { path        :: FilePath
    , permissions :: Permissions
    , size        :: Integer
    , fileCount   :: Integer -- ^ amount of files inside dir
    }
  deriving (Eq)

-- | Returns the file name by information
name :: Information -> FilePath
name = takeFileName . path

-- | Gets information about entity from disk
fromFilePath :: FilePath -> IO Information
fromFilePath p = do
  isFile <- doesFileExist p
  isDir <- doesDirectoryExist p
  if isFile
    then FileInfo <$>
      makeAbsolute p
      <*> getPermissions p
      <*> getFileSize p
      <*> (pure $ takeExtension p)
    else if isDir
      then DirInfo <$>
        makeAbsolute p
        <*> getPermissions p
        -- make it zeroes, it will count anyway
        <*> return 0
        <*> return 0
      else throw $ mkIOError doesNotExistErrorType "getInformation" Nothing (Just p)

-- | Deriving instance show for the Information
instance Show Information where
  show (FileInfo p per s e) = intercalate "\n"
    [ "Infomation about file \"" ++ takeFileName p ++ "\""
    , "Full path:         " ++ p
    , "Permissions:       " ++ show per
    , "Size:              " ++ show s ++ " bytes"
    , "Extension:         " ++ e
    ]

  show (DirInfo p per s c) = intercalate "\n"
    [ "Information about directory \"" ++ takeFileName p ++ "\""
    , "Full path:         " ++ p
    , "Permissions:       " ++ show per
    , "Size:              " ++ show s ++ " bytes"
    , "Files count:       " ++ show c
    ]

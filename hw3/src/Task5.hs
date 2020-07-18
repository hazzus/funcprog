module Task5
  ( FS (..)
  , upload
  , uploadRelative
  , name
  , contents
  , _File
  , _Dirname
  , _Dircont
  ) where

import           Control.Monad    ((>=>))
import           System.Directory (doesDirectoryExist, doesFileExist,
                                   listDirectory, makeAbsolute,
                                   withCurrentDirectory)
import           System.IO.Error  (ioError, userError)

import           Lens.Micro       (Lens', Traversal', lens)

-- | Data structure that si,ply represents the filesystem tree
data FS
  -- | Directory constructor of FS
  = Dir
  { _name     :: FilePath -- ^ get name of file or directory
  , _contents :: [FS] -- ^ get contents of directory
  }
  -- | File constructor of FS
  | File
  { _name :: FilePath
  }
  deriving (Show)

-- | uploads the FS from the real world by relative path
uploadRelative :: FilePath -> IO FS
uploadRelative = makeAbsolute >=> upload

-- | uploads the FS from the real world by absolute path
upload :: FilePath -> IO FS
upload root = do
  isDir <- doesDirectoryExist root
  if isDir
    then do
      content <- listDirectory root
      fss <- withCurrentDirectory root $ mapM upload content
      return $ Dir root fss
    else do
      exists <- doesFileExist root
      if exists
        then return $ File root
        else ioError $ userError "No such path"

-- Basic lens

-- | Lens to reach name of FS
name :: Lens' FS FilePath
name = lens _name $ \fs v -> fs {_name = v}

-- | Lens to reach contents of FS (fails on File)
contents :: Lens' FS [FS]
contents = lens _contents $ \fs v -> fs {_contents = v}

-- Basic prisms

-- | Traversal (Prism) for file contructor reaching it's name
_File :: Traversal' FS FilePath
_File f (File n) = File <$> f n
_File _ d        = pure d

-- | Traversal (Prism) for directory reaching it's name
_Dirname :: Traversal' FS FilePath
_Dirname f (Dir n c) = Dir <$> f n <*> pure c
_Dirname _ f         = pure f

-- | Traversal (Prism) for diractory reaching it's contents
_Dircont :: Traversal' FS [FS]
_Dircont f (Dir n c) = Dir n <$> f c
_Dircont _ f         = pure f


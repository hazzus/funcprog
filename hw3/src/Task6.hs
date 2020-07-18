{-# LANGUAGE Rank2Types #-}
module Task6
  ( cd
  , ls
  , file
  ) where

import           Lens.Micro (Traversal', each, filtered, (^?))

import           Task5      (FS (..), name, _Dircont, _Dirname, _File)

-- | cd is a complex traversal for changing directory
-- (looking inside dir from the content of current
cd :: FilePath -> Traversal' FS FS
cd p = _Dircont . each . filtered (elem p . (^? _Dirname))

-- | ls is a complex traversal for listing the names of contents of directory
ls :: Traversal' FS FilePath
ls = _Dircont . each . name

-- | file is a complex traversal for finding if file with given name exists
-- in a directory
file :: FilePath -> Traversal' FS FilePath
file p = _Dircont . each . filtered (elem p . (^? _File)) . name


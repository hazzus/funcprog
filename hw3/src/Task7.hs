module Task7
  ( changeExtension
  , lsr
  , rmd
  , rmf
  ) where

import           Control.Applicative (liftA2)
import           Lens.Micro          (Traversal', each, (%~), (&), (^..), (^?))
import           System.FilePath     (replaceExtension)

import           Task5               (FS (..), _Dircont, _Dirname, _File)

-- | Function that changes extension of every file in directory or
-- an extension of given file and returns a new FS
changeExtension :: String -> FS -> FS
changeExtension ne =
  (& _Dircont . each . _File %~ flip replaceExtension ne)

recurse :: Traversal' FS FilePath
recurse f (Dir n c) = liftA2 Dir (f n) (traverse (recurse f) c)
recurse f (File n)  = File <$> f n

-- | lsr listes a directory recursively
lsr :: FS -> [FilePath]
lsr = (^.. recurse)

rmcommon :: (FS -> Bool) -> FS -> FS
rmcommon matcher = (& _Dircont %~ filter (not . matcher))

-- | rmd removes an empty directory with a given name
-- from a given directory, otherwise it doesn't change FS
rmd :: FilePath -> FS -> FS
rmd p = rmcommon matching
  where
    matching e = helper _Dircont null e && helper _Dirname (== p) e
    helper t f = (any f) . (^? t)

-- bonus section

-- | rmf removes a file with given name from the given directory
rmf :: FilePath -> FS -> FS
rmf p = rmcommon $ (any (== p)) . (^? _File)

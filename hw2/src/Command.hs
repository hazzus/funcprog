module Command
  ( Command (..)
  , HitCommand (..)
  ) where

-- | Basic data type for all HIFM commands
data Command
  -- | Constructor for cd command -- change current directory
  = Cd  { path :: FilePath }
  -- | Constructor for dir command - show content of curdir
  | Dir { all  :: Bool -- ^ should affect everything or not
        }
  -- | Constructor for ls command - show content of path
  | Ls  { path :: FilePath
        , all  :: Bool
        }
  -- | Constructor for mkdir command -- create a directory
  | MkDir { paths   :: [FilePath]
          , parents :: Bool -- ^ create parents or not
          }
  -- | Constructor for rm command - remove path
  | Rm  { paths     :: [FilePath] -- ^ paths to affect
        , recursive :: Bool -- ^ recursive delete or not
        }
  -- | Constructor for mkfile command - create a file
  | MkFile { paths   :: [FilePath] }
  -- | Constructor for cat command - show content of file
  | Cat { file   :: FilePath  -- ^ file to affect
        }
  -- | Constructor for echoto command - append text to a file
  | EchoTo { file :: FilePath
           , text :: String -- ^ text to append to file
           }
  -- | Constructor for find command -- find a file by name
  | Find { name :: FilePath -- ^ name of file to find
         , whr  :: FilePath -- ^ where to search
         }
  -- | Constructor for info command - show info about file entity
  | Info { path :: FilePath }
  -- | Constructor for hit vcs command
  | Hit { hitCommand :: HitCommand -- ^ hit command to execute
        }
  -- | Fail message to print
  | Fail { message :: String -- ^ message of fail
         }
  deriving (Eq)

-- | Basic data type for commands of hit
data HitCommand
  -- | Constructor for init command - create a vcs in curdir
  = Init
  -- | Constructor for add command - add file for vcs
  | Add { adds :: [FilePath] -- ^ files to add
        }
  -- | Constructor for update command - add new revision
  | Update { comment :: String -- ^ comment for revision
           }
  -- | Constructor for log command - show history of revision
  | Log
  -- | Constructor for hit cat commant - show content of file of specified revision
  | HitCat { toCat    :: FilePath -- ^ file to cat
           , revision :: String -- ^ revision of file to cat
           }
  -- | Constructor for restore command - set state of curdir to specified revision
  | Restore { revision :: String
            }
  -- | Constructor for deinit command - remove vcs from curdir
  | Deinit
  deriving (Eq)

module HitParser
  ( hit
  ) where

import           Options.Applicative (Parser, command, fullDesc, help,
                                      hsubparser, info, metavar, progDesc, some,
                                      strArgument)

import           Command             (Command (Hit), HitCommand (..))

-- TODO support indexing, but DEADLINE IS CLOSE ?

-- | Parser for all 'Command.HitCommand' s
hit :: Parser Command
hit = Hit <$> hsubparser
  ( command "init" (info (pure Init) $
      fullDesc <> progDesc "initialize hit in current directory")
  <> command "add" (info hitAdd $
      fullDesc <> progDesc "[unsopported] add file or directory to a hit tracking")
  <> command "update" (info hitUpdate $
      fullDesc <> progDesc "make a revision out of a current state of current directory")
  <> command "log" (info (pure Log) $
      fullDesc <> progDesc "show all revisions with comments")
  <> command "cat" (info hitCat $
      fullDesc <> progDesc "show file version of specified revision")
  <> command "restore" (info hitRestore $
      fullDesc <> progDesc "restore the current folder to specified revision")
  <> command "deinit" (info (pure Deinit) $
      fullDesc <> progDesc "deinitialize hit in current directory")
  )

hitAdd :: Parser HitCommand
hitAdd = Add <$> some
  ( strArgument
    ( metavar "PATH"
    <> help "paths to add to csv"
    )
  )

hitUpdate :: Parser HitCommand
hitUpdate = Update <$>
  strArgument
    ( metavar "COMMENT"
    <> help "comment for revision"
    )

hitCat :: Parser HitCommand
hitCat = HitCat <$>
  strArgument
    ( metavar "FILE"
    <> help "file to show content of"
    )
  <*> strArgument
    ( metavar "REVISION"
    <> help "revision of file to show"
    )

hitRestore :: Parser HitCommand
hitRestore = Restore <$>
  strArgument
    ( metavar "REVISION"
    <> help "revision to restore"
    )

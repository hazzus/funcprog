module MainArgsParser
  ( execArgParserInfo
  ) where

import           Options.Applicative (Parser, ParserInfo, execParser, fullDesc,
                                      header, help, helper, info, metavar,
                                      progDesc, strArgument, value, (<**>))

-- | Parser for the command-line run argument
-- (parses real arguments when launching HIFM -- the path to start from
execArgParserInfo :: IO FilePath
execArgParserInfo = execParser argParserInfo

argParserInfo :: ParserInfo FilePath
argParserInfo = info (argParser <**> helper) $
  fullDesc
  <> header "HIFM - Hifm Is more than a File Manager (Haskell-powered Interactive File Manager)"
  <> progDesc "HIFM allows you to easily operate with files and directories and also has an integrated VCS.\n For getting command help try: COMMAND --help"


argParser :: Parser FilePath
argParser = strArgument
  ( metavar "ROOT"
  <> value "."
  <> help "Root of virtual filesystem, place where the program will be launched"
  )

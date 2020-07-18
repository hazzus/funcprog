module CommandParser
  ( parseCommand,
    wordsCorrect
  ) where

import           Data.Char                 (isSpace)
import           Options.Applicative       (Parser, ParserInfo, ParserResult,
                                            argument, command, defaultPrefs,
                                            execParserPure, fullDesc, header,
                                            help, hidden, hsubparser, info,
                                            long, metavar, progDesc, short,
                                            some, strArgument, strOption,
                                            switch, value, (<**>))
import           Options.Applicative.Types (ParseError (ShowHelpText),
                                            readerAbort)

import           Command                   (Command (..))
import           HitParser                 (hit)

-- | Parser for the command line input
parseCommand :: [String] -> ParserResult Command
parseCommand = execParserPure defaultPrefs commandParserInfo

-- | Splits the string with quotes management, returns Left if there is an unmatched quote
wordsCorrect :: String -> Either String [String]
wordsCorrect inp = do
  let (f, s) = break (\x -> isSpace x || x == '"') inp
  case s of
    [] -> return [f]
    (c : cs) | isSpace c -> wordsCorrect (dropWhile (isSpace) s) >>= return . (f :)
             | otherwise -> do
                let (f', s') = break (== '"') $ cs
                case s' of
                  [] -> Left "Unmatched quote" -- undefined -- wait or fail
                  x | x == "\"" -> return [f, f']
                    | otherwise -> wordsCorrect (tail s') >>= return . ([f, f'] ++)
  >>= return . filter (/= "")

commandParserInfo :: ParserInfo Command
commandParserInfo = info (commandParser <**> helper) $
  fullDesc
  <> header "HIFM - Hifm Is more than a File Manager (Haskell-powered Interactive File Manager)"
  <> progDesc "HIFM allows you to easily operate with files and directories and also has an integrated VCS.\n For getting command help try: COMMAND --help"

-- Actually, parsers for every single command

commandParser :: Parser Command
commandParser = hsubparser
  ( command "cd"  (info cd  $
      fullDesc <> progDesc "change current directory")
  <> command "dir" (info dir $
      fullDesc <> progDesc "list contents current directory")
  <> command "ls"  (info ls  $
      fullDesc <> progDesc "list contents of specified directory")
  <> command "mkdir" (info mkdir $
      fullDesc <> progDesc "create some directories")
  <> command "rm" (info rm $
      fullDesc <> progDesc "remove a files or directories")
  <> command "mkfile" (info mkfile $
      fullDesc <> progDesc "create some files")
  <> command "cat" (info catParser $
      fullDesc <> progDesc "output content of file")
  <> command "echoto" (info echoto $
      fullDesc <> progDesc "write text to a file")
  <> command "find" (info findParser $
      fullDesc <> progDesc "find files by name")
  <> command "info" (info information $
      fullDesc <> progDesc "show information about file or directory")
  <> command "s-hit" (info hit $
      fullDesc
      <> header "s-HIT - hit or miss, i guess they never miss, huh?"
      <> progDesc "s-HIT - a Superior Haskell gIT.\
                 \ Simple snapshot-based vcs, can only operate with directories (and cat files by revision).\
                 \ Actually does not satisfy the task requirements but it is sort of a thing\
                 \ I managed to write before deadline")
  )

helper :: Parser (a -> a)
helper =
  argument (readerAbort ShowHelpText)
    ( value id
    <> metavar "help"
    <> help "show this help message"
    <> hidden
    )

cd :: Parser Command
cd = Cd <$> strArgument (metavar "PATH" <> help "directory to go to")

dir :: Parser Command
dir = Dir <$>
  switch
    ( long "all"
    <> short 'a'
    <> help "show hidden"
    )

ls :: Parser Command
ls = Ls <$>
  strArgument
    ( metavar "PATH"
    <> value "."
    <> help "directory to show contents"
    )
  <*> switch ( short 'a' <> help "show hidden" )
  -- <*> switch ( short 'R' )

mkdir :: Parser Command
mkdir = MkDir <$>
  some (strArgument (metavar "PATHS" <> help "directory to create"))
  <*> switch
    ( long "parents"
    <> short 'p'
    <> help "no error if existing, make parent directories as needed"
    )

rm :: Parser Command
rm = Rm <$>
  some (strArgument (metavar "PATHS" <> help "remove the PATHS"))
  <*> switch
    ( long "recursive"
    <> short 'r'
    <> help "remove dirs and their content revursively"
    )

mkfile :: Parser Command
mkfile = MkFile <$>
  some (strArgument $ metavar "PATHS" <> help "files to create")

catParser :: Parser Command
catParser = Cat <$>
  strArgument (metavar "FILE" <> help "file content to print")

echoto :: Parser Command
echoto = EchoTo <$>
  strArgument
    ( metavar "FILE"
    <> help "file to write to"
    )
  <*> strArgument
    ( metavar "TEXT"
    <> help "text to write to"
    )

findParser :: Parser Command
findParser = Find <$>
  strArgument (metavar "NAME" <> help "file-name to find")
  <*> strOption
    ( long "where"
    <> short 'w'
    <> metavar "PATH"
    <> value "."
    <> help "where to search"
    )

information :: Parser Command
information = Info <$>
  strArgument (metavar "PATH" <> help "filesystem entity to show info of")

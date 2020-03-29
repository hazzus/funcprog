module Hard
  ( listlistParser
  , listParser
  , omitWS
  ) where

import Base (element, eof, ok, satisfy)
import Control.Applicative ((<|>))
import Data.Char (isSpace)
import Parser (Parser (..))
import Simple (parseInteger)

-- | Parser that skips whitespaces
omitWS :: Parser Char ()
omitWS = (satisfy isSpace *> omitWS) <|> ok

parseList :: Parser Char a -> Int -> Parser Char [a]
parseList entityParser len =
  let parseRest = parseList entityParser
  in
    if len > 0
    then omitWS *> element ',' *> omitWS *> ((:) <$> entityParser) <*> (parseRest (len - 1))
    else [] <$ omitWS

-- | Parser which parser a list of integers
listParser :: Parser Char [Int]
listParser = omitWS *> (parseInteger >>= (parseList parseInteger))

-- | Parser which parses list of lists of integers
-- ignores all integer overplacement (even at end)
-- Grammar:
-- O = X O | ""
-- X = I , L
-- L = L' (I times)
-- L' = I' , L''
-- L'' = Num (I' times)
listlistParser :: Parser Char [[Int]]
listlistParser = omitWS *> (parseInteger >>= (parseList listParser)) <* eof


module Simple
  ( parseBrackets
  , parseInteger
  , parseIntegerOnly
  ) where

import Base (element, eof, ok, satisfy)
import Control.Applicative ((<|>))
import Data.Char (isDigit, ord)
import Parser (Parser (..))

-- Task 3

-- | Parser of correct brackets sequence
-- Brackets grammar:
-- S = IS | ""
-- I = (S)
parseBrackets :: Parser Char ()
parseBrackets = s <* eof
  where
    s = (i >> s) <|> ok
    i = element '(' >> s >> element ')'

-- | Parses integer with possible + or - before it
-- Grammar:
-- I = S L
-- S = + | - | ""
-- L = D L | ""
-- D = 0 1 2 3 4 5 6 7 8 9
parseInteger :: Parser Char Int
parseInteger = (s <*> l)  -- <* eof
  where
    s = (snd <$ element '+') <|> ((negate . snd) <$ element '-') <|> (snd <$ ok)
    l = ((uprise <$> d) <*> l) <|> ((0, 0) <$ ok)
    d = fmap (\c -> ord c - ord '0') (satisfy isDigit)

    uprise :: Int -> (Int, Int) -> (Int, Int)
    uprise x (y, res) = (y + 1, x * 10 ^ y + res)

-- | Does the same thing as parseInteger but fails
-- if something remains at the end of string (not part of number)
parseIntegerOnly :: Parser Char Int
parseIntegerOnly = parseInteger <* eof

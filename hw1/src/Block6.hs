module Block6
  ( Parser (..)
  , ok
  , eof
  , satisfy
  , element
  , stream
  , parseBrackets
  ) where

import Control.Applicative

-- Task 1

-- | Type representing a parser which can parse any type stream
data Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

-- | Deriving Functor for Parser
instance Functor (Parser s) where
  -- fmap :: (a -> b) -> Parser s a -> Parser s b
  -- | fmaps a function to every parsing result
  fmap f (Parser run) =
    Parser (\inp ->
    run inp >>= (\(a, rest) -> pure (f a, rest)))

-- | Deriving Applicative for Parser
instance Applicative (Parser s) where
  -- pure :: a -> Parser s a
  -- | Constructs an Applicative
  pure x = Parser (\inp -> Just (x, inp))

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  -- | Applies a function parsed to a parsed elements
  (<*>) (Parser frun) (Parser run) =
    Parser (\inp ->
      frun inp >>= (\(f, t) ->
        run t >>= (\(a, r) ->
          pure (f a, r))))

-- | Deriving Monad for Parser
instance Monad (Parser s) where
  -- (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  (>>=) (Parser run) f =
    Parser (\inp ->
    run inp >>= (\(x, r) -> runParser (f x) r))

-- | Deriving Alternative for Parser
instance Alternative (Parser s) where
  empty = Parser (const Nothing)

  -- (<|>) :: Parser s a -> Parser s a -> Parser s a
  (<|>) (Parser x) (Parser y) =
    Parser (\inp -> x inp <|> y inp)

-- Task 2

-- | Parser combinator that accepts anything
ok :: Parser s ()
ok = Parser (\inp -> Just ((), inp))

-- | Parser combinator that goes through the stream
-- till the end
eof :: Parser s ()
eof =
  let checkEOF [] = Just ((), [])
      checkEOF _  = Nothing
  in Parser checkEOF

-- | Parser combinator that parses only elements from
-- stream that satisfy a predicate
satisfy :: (s -> Bool) -> Parser s s
satisfy pr =
  let checkPredicate []       = Nothing
      checkPredicate (x : xs) | pr x    = Just (x, xs)
                              | otherwise = Nothing
  in Parser checkPredicate

-- | Parser combinator that parses only symbols same
-- as given one
element :: Eq s => s -> Parser s s
element e = satisfy (== e)

-- | Parser combinator that parses only parts of list
-- which is same as given one
stream :: Eq s => [s] -> Parser s [s]
stream = traverse element

-- Task 3

-- | Parser of correct brackets sequence
-- Brackets grammar:
-- S = NE | E
-- NE = I S
-- I = ( S )
-- E = "" (aka OK)
parseBrackets :: Parser Char ()
parseBrackets = s <* eof
  where
    s = i *> s <|> ok
    i = element '(' *> s <* element ')'

module Parser
  ( Parser (..)
  ) where

import Control.Applicative (Alternative (..))

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
  pure x = Parser (\inp -> pure (x, inp))

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  -- | Applies a function parsed to a parsed elements
  (<*>) (Parser frun) (Parser run) =
    Parser (\inp -> do
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
  -- (<|>) (Parser x) (Parser y) =
    -- Parser (\inp -> x inp <|> y inp)
  (<|>) p p' =
    Parser (\inp -> runParser p inp <|> runParser p' inp)


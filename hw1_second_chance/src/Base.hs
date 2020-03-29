module Base
  ( element
  , eof
  , ok
  , satisfy
  , stream
  ) where

import Parser (Parser (..))

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


module Block3
  ( NonEmpty (..)
  , ThisOrThat (..)
  , maybeConcat
  ) where

import Data.Maybe (catMaybes)

-- Task 1

-- | Function which unwraps maybes under list
-- skipping Nothing's
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat = (foldr (<>) []) . catMaybes

-- Task 2

-- | Type representing a non-empty list
data NonEmpty a = a :| [a] deriving (Show)

-- | Deriving Semigroup for NonEmpty
instance Semigroup (NonEmpty a) where
  (<>) (x :| xs) (y :| ys) = x :| (xs ++ y : ys)

-- | Deriving Eq for NonEmpty
instance (Eq a) => Eq (NonEmpty a) where
  (==) (x :| xs) (y :| ys) = (x == y) && (xs == ys)

-- | Type which can hold one of two types of both of them
data ThisOrThat a b = This a | That b | Both a b deriving (Show)

-- | Deriving Semigroup for ThisOrThat
instance (Semigroup a, Semigroup b) => Semigroup (ThisOrThat a b) where
  (<>) (This a)   (This b)   = This (a <> b)
  (<>) (This a)   (That b)   = Both a b
  (<>) (This a)   (Both b c) = Both (a <> b) c
  (<>) (That a)   (That b)   = That (a <> b)
  (<>) (That a)   (Both b c) = Both b (a <> c)
  (<>) (Both a b) (Both c d) = Both (a <> c) (b <> d)
  (<>) a          b          = b <> a

-- | Deriving Eq for ThisOrThat
instance (Eq a, Eq b) => Eq (ThisOrThat a b) where
  (==) (This a)   (This b)   = a == b
  (==) (That a)   (That b)   = a == b
  (==) (Both a b) (Both c d) = a == c && b == d
  (==) _          _          = False

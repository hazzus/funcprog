module Block4
  ( Tree (..)
  , NonEmpty (..)
  , stringSum
  ) where

import Control.Applicative (liftA2)
import Text.Read (readMaybe)

-- Task 1

-- | Function which sums all Integers in a string
-- separated by whitespaces, if illegal character appears,
-- returns Nothing
stringSum :: String -> Maybe Int
stringSum s = sum <$> (traverse readMaybe $ words s)

-- Task 2

-- | A simpler Tree type wich can hold values
-- only in leafs
data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a

-- | Deriving Functor for Tree
instance Functor Tree where
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)
  fmap f (Leaf x)     = Leaf (f x)

-- | Deriving Applicative for Tree
instance Applicative Tree where
  pure = Leaf

  (<*>) (Branch l r) t = Branch (l <*> t) (r <*> t)
  (<*>) (Leaf x) t     = x <$> t

-- | Deriving Foldable for Tree
instance Foldable Tree where
  foldMap f (Branch l r) = foldMap f l <> foldMap f r
  foldMap f (Leaf x)     = f x

  foldr f s (Branch l r) = foldr f (foldr f s r) l
  foldr f s (Leaf x)     = f x s

-- | Deriving Traversable for Tree
instance Traversable Tree where
  traverse f (Branch l r) = liftA2 Branch (traverse f l) (traverse f r)
  traverse f (Leaf x)     = Leaf <$> f x

-- Task 3

-- | Type representing a non-empty list
data NonEmpty a = a :| [a]

-- | Operator for concatenating NonEmpty and List
(++|) :: NonEmpty a -> [a] -> NonEmpty a
(x :| xs) ++| ys = x :| (xs ++ ys)

-- | Deriving Functor for NonEmpty
instance Functor NonEmpty where
  fmap f (x :| xs) = f x :| fmap f xs

-- | Deriving Applicative for NonEmpty
instance Applicative NonEmpty where
  pure = (:| [])

  (<*>) (f :| fs) (x :| xs) = (f x) :| ((fs <*> xs) ++ (fs <*> (x : xs)))

-- | Deriving Monad for NonEmpty
instance Monad NonEmpty where
  (>>=) (x :| xs) f = (f x) ++| (xs >>= toList . f)
    where toList (y :| ys) = y : ys

-- | Deriving Foldable for NonEmpty
instance Foldable NonEmpty where
  foldMap f (x :| xs) = mappend (f x) (foldMap f xs)

  foldr f x (y :| ys) = f y $ foldr f x ys

-- | Deriving Traversable for NonEmpty
instance Traversable NonEmpty where
  traverse f (x :| xs) = liftA2 (:|) (f x) (traverse f xs)


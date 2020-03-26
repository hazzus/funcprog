module Block1
  ( Nat(..)
  , Tree(..)
  , WeekDay(..)
  , isEven
  , nextDay
  , afterDays
  , isWeekend
  , daysToParty
  , empty
  , size
  , find
  , insert
  , fromList
  , toList
  , remove
  ) where

import Data.List.NonEmpty (NonEmpty (..), (<|))

-- Task 1

-- | Type representing a weekday
data WeekDay
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show)

-- | Implementation of Enum instance for weekday
instance Enum WeekDay where
  -- | Converts WeekDay to Integer
  -- returns the number of weekday from Monday
  -- starting with zero
  fromEnum day =
    case day of
      Monday    -> 0
      Tuesday   -> 1
      Wednesday -> 2
      Thursday  -> 3
      Friday    -> 4
      Saturday  -> 5
      Sunday    -> 6
  -- | Converting Integer to WeekDay
  -- returns a WeekDay by it's number
  -- from Monday starting with zero
  toEnum iday =
    case iday of
      0 -> Monday
      1 -> Tuesday
      2 -> Wednesday
      3 -> Thursday
      4 -> Friday
      5 -> Saturday
      6 -> Sunday
      _ -> error "Cannot be a weekday, if not less than 7"

-- | Implementation of instance Eq for WeekDay
instance Eq WeekDay where
  (==) Monday    Monday    = True
  (==) Tuesday   Tuesday   = True
  (==) Wednesday Wednesday = True
  (==) Thursday  Thursday  = True
  (==) Friday    Friday    = True
  (==) Saturday  Saturday  = True
  (==) Sunday    Sunday    = True
  (==) _         _         = False

-- | Function returning a next WeekDay after passed
-- e.g. after Sunday comes Monday
nextDay :: WeekDay -> WeekDay
nextDay d = toEnum $ mod (1 + fromEnum d) 7

-- | Function returning a WeekDay after some
-- amount of days passed as second argument
afterDays :: WeekDay -> Int -> WeekDay
afterDays d x = toEnum $ mod (x + fromEnum d) 7

-- | Function returning true if WeekDay is weekend
isWeekend :: WeekDay -> Bool
isWeekend day =
  case day of
    Saturday -> True
    Sunday   -> True
    _        -> False

-- | Function returning days till Friday
daysToParty :: WeekDay -> Int
daysToParty day = if diff < 0 then diff + 7 else diff
  where diff = fromEnum Friday - fromEnum day

-- Task 2

-- | Type representing recursive Natural number
data Nat = Z | S Nat deriving (Show)

-- | Implementation of instance Num for Nat
instance Num Nat where
  -- | Sums two Nat
  (+) x Z     = x
  (+) x (S y) = S (x + y)

  -- | Multiplies two Nat
  (*) _ Z     = Z
  (*) x (S y) = x + (x * y)

  -- | Substracts two Nats
  -- Zero - anything is Zero
  (-) x     Z     = x
  (-) Z     _     = Z
  (-) (S x) (S y) = x - y

  -- | Converts Integer to Nat
  fromInteger 0 = Z
  fromInteger x | x > 0 = S (fromInteger (x - 1))
                | otherwise = error "Negative Integer cannot be Natural"

  -- | Takes absolute value of Nat
  abs = id

  -- | Sign of Nat
  signum Z = 0
  signum _ = 1

-- | Checking if Nat
isEven :: Nat -> Bool
isEven (S (S x)) = isEven x
isEven (S Z)     = False
isEven Z         = True

-- | Implementation of instance Eq for Nat
instance Eq Nat where
  (==) Z     Z     = True
  (==) (S x) (S y) = x == y
  (==) _     _     = False

-- | Implementation of instance Ord for Nat
instance Ord Nat where
  (<=) Z     _     = True
  (<=) (S _) Z     = False
  (<=) (S x) (S y) = x <= y

-- Task 3

-- | Type which represents a tree, each Node
-- of which contains a NonEmpty list of the same values
data Tree a
  = Node (NonEmpty a) (Tree a) (Tree a)
  | Leaf deriving (Show)

-- | Deriving instance Eq for Tree
instance (Eq a) => Eq (Tree a) where
  (==) Leaf         Leaf           = True
  (==) (Node x l r) (Node y l' r') = (x == y) && (l == l') && (r == r')
  (==) _            _              = False

-- | Function returns rather Tree is empty or not
empty :: Tree a -> Bool
empty Leaf = True
empty _    = False

-- | Function returns a size of Tree
size :: Tree a -> Int
size (Node x l r) = length x + size l + size r
size Leaf         = 0

-- | Function returns a node of a Tree which contains
-- an element passed. Needs a tree to be a search-tree.
-- returns Nothing if no such element
find :: Ord a => Tree a -> a -> Maybe (Tree a)
find t@(Node (x :| _) l r) e =
  if x == e
    then Just t
    else if e < x
      then find l e
      else find r e
find Leaf _ = Nothing

-- | Function which returns a new Tree with an inserted
-- element. Tree must be a BST.
insert :: Ord a => Tree a -> a -> Tree a
insert (Node xs@(x :| _) l r) e =
  if x == e
    then Node (e <| xs) l r
    else if e < x
      then Node xs (insert l e) r
      else Node xs l (insert r e)
insert Leaf e = Node (e :| []) Leaf Leaf

-- | Converts a list to a BST inside Tree.
fromList :: Ord a => [a] -> Tree a
fromList = foldl (insert) Leaf

-- | Returns a new Tree with removed element.
-- Needs a Tree to be BST.
remove :: Ord a => Tree a -> a -> Tree a
remove n@(Node ls@(x :| xs) l r) e =
  if x == e
    then
      case xs of
        []       -> realRemove n
        el : els -> Node (el :| els) l r
    else if e < x
      then Node ls (remove l e) r
      else Node ls l (remove r e)
  where
    realRemove :: Ord a => Tree a -> Tree a
    realRemove (Node _ l' r') =
      case getLeftest r' of
        Leaf           -> l'
        Node x'' _ r'' -> Node x'' l' r''
    realRemove _ = error "Impossible happened"

    getLeftest :: Ord a => Tree a -> Tree a
    getLeftest (Node x' l' r') =
      case getLeftest l' of
        Leaf           -> Node x' l' r'
        Node x'' _ r'' -> Node x'' l' (Node x' r'' r')
    getLeftest Leaf = Leaf
remove Leaf _ = Leaf

-- Block 2 Task 1

-- | Deriving Foldable for Tree from Block 1
instance Foldable Tree where
  -- foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f x (Node e l r) = foldr f (foldr f (foldr f x r) e) l
  foldr _ x Leaf         = x

  -- foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f (Node e l r) = foldMap f l <> foldMap f e <> foldMap f r
  foldMap _ Leaf         = mempty

-- for property testing
toList :: Tree a -> [a]
toList = foldr (:) []

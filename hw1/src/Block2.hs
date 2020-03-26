{-# LANGUAGE InstanceSigs #-}

module Block2
  ( splitOn
  , joinWith
  ) where

import Data.List.NonEmpty (NonEmpty (..))

-- Task 2

-- | Function which splits a list by a delimeter
splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn d = foldr helper ([] :| [])
  where
    helper cur (x :| xs) =
      if cur == d
      then [] :| (x : xs)
      else (cur : x) :| xs

-- | Function reverse to a splitOn
joinWith :: a -> NonEmpty [a] -> [a]
joinWith d (x :| xs) = x ++ foldr (\c o -> d : (c ++ o)) [] xs

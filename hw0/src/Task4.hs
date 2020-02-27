module Task4
  ( factorial
  , fibonacci
  , iterateElement
  , mapFix
  ) where

import Data.Function (fix)

-- | 'iterateElement' is endless list generator
iterateElement :: a -> [a]
iterateElement x = fix (x :)

-- | 'fibonacci' finds n'th fibonacci element
fibonacci :: Integer -> Integer
fibonacci = fix fibImpl
  where
    fibImpl
        :: (Integer -> Integer)
        -> Integer
        -> Integer
    fibImpl f x =
      if x <= 1
      then 1
      else f (x - 1) + f (x - 2)

-- | 'factorial' finds n'th factorial
factorial :: Integer -> Integer
factorial = fix facImpl
  where
    facImpl
        :: (Integer -> Integer)
        -> Integer
        -> Integer
    facImpl f x =
      if x <= 0
      then 1
      else x * f (x - 1)

-- | 'mapFix' is map implementation of map
mapFix :: (a -> b) -> [a] -> [b]
mapFix = fix mapFixImpl
  where
    mapFixImpl
        :: ((a -> b) -> [a] -> [b])
        -> (a -> b)
        -> [a]
        -> [b]
    mapFixImpl f m (x : xs) = m x : f m xs
    mapFixImpl _ _ []       = []


{-# LANGUAGE BangPatterns #-}
module Task1
  ( Point (..)
  , crossProduct
  , doubleArea
  , minus
  , perimeter
  , plus
  , scalarProduct
  ) where

-- Geometry task

import           Control.DeepSeq (NFData, deepseq, rnf)

-- | Structure representing a point in 2D
data Point
  = Point !Int !Int
  deriving (Show, Eq)

-- | Instance for nf computation of Point
instance NFData Point where
  rnf !(Point x y) = x `seq` y `seq` ()

-- | Function for adding two Points
plus :: Point -> Point -> Point
plus (Point x1 y1) (Point x2 y2) =
  let !x = x1 + x2
      !y = y1 + y2
  in Point x y

-- | Function for substracting one Point from another
minus :: Point -> Point -> Point
minus (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

-- | Scalar product of points
scalarProduct :: Point -> Point -> Int
scalarProduct (Point x1 y1) (Point x2 y2) = x1 * x2 + y1 * y2

-- | Cross (pseudo-scalar) product of Points
crossProduct :: Point -> Point -> Int
crossProduct (Point x1 y1) (Point x2 y2) = x1 * y2 - x2 * y1

vectorLength :: Point -> Point -> Double
vectorLength a b  =
  let d = minus a b
  in deepseq d $ sqrt $ fromIntegral $ scalarProduct d d

helper :: Num a => (Point -> Point -> a) -> [Point] -> a
helper !f (x : xs) = inner x xs 0
  where
    inner st (nxt : othr) !acc = inner nxt othr $ acc + f st nxt
    inner st []           !acc = f st x + acc
helper _ [] = 0

-- | Functions that calculates the perimeter of a polygon
perimeter :: [Point] -> Double
perimeter = helper vectorLength

-- | Function that calculates the area of polygon multiplied by two
doubleArea :: [Point] -> Int
doubleArea = abs . helper crossProduct


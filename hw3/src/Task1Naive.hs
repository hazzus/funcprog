module Task1Naive
  ( Point (..)
  , crossProduct
  , doubleArea
  , minus
  , perimeter
  , plus
  , scalarProduct
  ) where

-- Geometry task

import           Task1 (Point (..))

-- | Function for adding two Points
plus :: Point -> Point -> Point
plus (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

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
  in  sqrt $ fromIntegral $ scalarProduct d d


helper :: Num a => (Point -> Point -> a) -> [Point] -> a
helper f (x : xs) = inner x xs
  where
    inner st (nxt : othr) = f st nxt + inner nxt othr
    inner st []           = f st x
helper _ [] = 0

-- | Functions that calculates the perimeter of a polygon
perimeter :: [Point] -> Double
perimeter = helper vectorLength

-- | Function that calculates the area of polygon multiplied by two
doubleArea :: [Point] -> Int
doubleArea = abs . helper crossProduct

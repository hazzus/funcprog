module Task6
  ( harold
  , haroldWHNF
  , poleChudes
  , poleChudesWHNF
  ) where

import Data.Maybe (mapMaybe)
import Prelude hiding (null)
import Task1 (distributivity)

foo :: Char -> Maybe Double
foo char =
  case char == 'o' of
    True  -> Just $ exp pi
    False -> Nothing

-- not actually, but good enough for this task
null :: [a] -> Bool
null [] = True
null _  = False

-- | Returns two sentenses about harold wrapped in Either and tuple
harold :: (Either String a, Either String b)
harold = distributivity (Left ("harold" ++ "hide" ++ "the" ++ "pain"))

-- | Weak Head Normal Form for 'harold'
haroldWHNF :: (Either String a, Either String b)
haroldWHNF = (Left ("harold" ++ " hide " ++ "the " ++ "pain"), Left ("harold" ++ " hide " ++ "the " ++ "pain"))

-- | Quite a hard way to get False
poleChudes :: Bool
poleChudes = null $ mapMaybe foo "pole chudes ochen' chudesno"

-- | Weak Head Normal Form of 'poleChudes'
poleChudesWHNF :: Bool
poleChudesWHNF = False


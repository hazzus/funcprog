{-# LANGUAGE TypeOperators #-}

module Task1
  ( associator
  , distributivity
  , eitherAssoc
  ) where

-- | The 'distributivity' function transforms Either with right type as pair
-- into pair of Either (with 2 lefts or unwrapped pair)
distributivity
  :: Either a (b, c)
  -> (Either a b, Either a c)
distributivity (Left x)       = (Left x, Left x)
distributivity (Right (x, y)) = (Right x, Right y)

-- | The 'associator' function represents assosiativity
-- in tuples
associator
  :: (a, (b, c))
  -> ((a, b), c)
associator (x, (y, z)) = ((x, y), z)

type (<->) a b = (a -> b, b -> a)

-- | The 'eitherAssoc' function combines associativity with
-- Either unwrapping
eitherAssoc
  :: Either a (Either b c)
  <-> Either (Either a b) c
eitherAssoc = (left, right)
  where
    left
        :: Either a (Either b c)
        -> Either (Either a b) c
    left (Left x)          = Left (Left x)
    left (Right (Left x))  = Left (Right x)
    left (Right (Right x)) = Right x

    right
        :: Either (Either a b) c
        -> Either a (Either b c)
    right (Left (Left x))  = Left x
    right (Left (Right x)) = Right (Left x)
    right (Right x)        = Right (Right x)


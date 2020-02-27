module Task5
    ( Nat
    , churchMul
    , churchPlus
    , churchToInt
    , succChurch
    , zero
    ) where

-- | type 'Nat' represents Church numeral
type Nat a = (a -> a) -> a -> a

-- | zero in Church numerals
zero :: Nat a
zero _ x = x

-- | +1 in Church numerals
succChurch :: Nat a -> Nat a
succChurch n f x = f (n f x)

-- | + in Church numerals
churchPlus :: Nat a -> Nat a -> Nat a
churchPlus n m f x = n f (m f x)

-- | * in Church numerals
churchMul :: Nat a -> Nat a -> Nat a
churchMul n m f x = n (m f) x

-- | transformes church numeral into Integer
churchToInt :: Nat Integer -> Integer
churchToInt n = n (+ 1) 0


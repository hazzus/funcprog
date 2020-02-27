module Task2
  ( doubleNeg
  , doubleNegElim
  , excludedNeg
  , pierce
  , thirdNegElim
  ) where

import Data.Void (Void)

type Neg a = a -> Void

-- | 'doubleNeg' function is representation of simple axiom-schema in intuitionistic logic
-- where Neg == -> Void
doubleNeg :: a -> Neg (Neg a) -- extends as a -> (a -> Void) -> Void
doubleNeg x y = y x

-- | 'excludedNeg' is axiom-schema allowing to exlude Negation
excludedNeg :: Neg (Neg (Either a (Neg a))) -- extends as (Either a (a -> Void)) -> Void -> Void
excludedNeg x = (x . Right) (x . Left)

-- | 'pierce' is errored in intuitionistic logic function
pierce :: ((a -> b) -> a) -> a
pierce = error "Unable to populate a type"
-- Why? В ИИВ необщезначимо

-- | 'doubleNegElim' also unpopulated function in intuitionistic logic
doubleNegElim :: Neg (Neg a) -> a -- extends as ((a -> Void) -> Void) -> a
doubleNegElim = error "Unable to populate a type"
-- Why? В ИИВ необщезначимо

-- | 'thirdNegElim' exludes 2 negations of three
thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim x = x . doubleNeg


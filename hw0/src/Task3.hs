module Task3
  ( composition
  , contraction
  , identity
  , permutation
  , s
  ) where

-- | The 's' is just S-combinator
s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

-- | 'composition' is composition implementation via SK-basis
composition :: (b -> c) -> (a -> b) -> a -> c
composition = s (const s) const

-- | 'identity' is id function implemented via SK-basis
identity :: a -> a
identity = s const const

-- | 'contraction' is contraction implementation via SK-basis
contraction :: (a -> a -> b) -> a -> b
contraction = s s (s const)

-- | 'permutation' is permuatation implementation via SK-basis
permutation :: (a -> b -> c) -> b -> a -> c
permutation = s (s (const (s (const s) const)) s) (const const)


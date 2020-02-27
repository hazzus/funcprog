{-# LANGUAGE ScopedTypeVariables #-}

module Task7
  ( first
  , second
  , third
  ) where

import Data.Either (lefts, rights)

-- | Oh, again, 'first' is a way to get False for masochists
first :: Bool
first = -- null . head $ map (uncurry id) [((++) "Dorian ", " Grey")]
  let
    t1 = " Grey" :: String
    t2 = "Dorian " :: String
    t3 = (++) :: String -> String -> String
    t4 = t3 t2 :: String -> String
    t5 = (t4, t1) :: (String -> String, String)
    t6 = [t5] :: [(String -> String, String)]
    t7 = id :: (String -> String) -> String -> String
    t8 = uncurry
        :: ((String -> String) -> String -> String)
        -> (String -> String, String) -> String
    t9 = t8 t7 :: (String -> String, String) -> String
    t10 = map
        :: ((String -> String, String) -> String)
        -> [(String -> String, String)]
        -> [String]
    t11 = t10 t9
        :: [(String -> String, String)]
        -> [String]
    t12 = t11 t6 :: [String]
    t13 = ($)
        :: ([String] -> Bool)
        -> [String]
        -> Bool
    t14 = head :: [String] -> String
    t15 = (.)
        :: (String -> Bool)
        -> ([String] -> String)
        -> ([String] -> Bool)
    t16 = null :: String -> Bool
    t17 = t15 t16
        :: ([String] -> String)
        -> ([String] -> Bool)
    t18 = t17 t14 :: [String] -> Bool
    t19 = t13 t18 :: [String] -> Bool
  in t19 t12

-- | 'second' function returns [(3, 64)]
second :: forall a b. (Num a, Num b) => [(a, b)]
second = t15 t8 -- (\x -> zip (lefts x) (rights x)) [Left (1 + 2), Right (2 ^ 6)]
  where
    t15 :: forall a1 b1. (Num a1, Num b1) => [Either a1 b1] -> [(a1, b1)]
    t15 x =
      let
        t9 = rights :: (Num a1, Num b1) => [Either a1 b1] -> [b1]
        t10 = lefts :: (Num a1, Num b1) => [Either a1 b1] -> [a1]
        t11 = zip :: (Num a1, Num b1) => [a1] -> [b1] -> [(a1, b1)]
        t12 = t9 x :: [b1]
        t13 = t10 x :: [a1]
        t14 = t11 t13 :: [b1] -> [(a1, b1)]
      in
        t14 t12

    t1 = 6 :: Integer
    t2 = 2 :: b
    t3 = (^) :: b -> Integer -> b
    t4 = t3 t2 :: Integer -> b
    t5 = t4 t1 :: b
    t6 = Right :: b -> Either a b
    t7 = t6 t5 :: Either a b
    t1' = 2 :: Num a => a
    t2' = 1 :: Num a => a
    t3' = (+) :: (Num a) => a -> a -> a
    t4' = t3' t2' :: ( Num a) => a -> a
    t5' = t4' t1' :: Num a => a
    t6' = Left :: (Num a) => a -> Either a b
    t7' = t6' t5' :: (Num a) => Either a b
    t8 = [t7', t7] :: [Either a b]

{-
 let impl = \x y -> not x || y in
    let isMod2 = \x -> x `mod` 2 == 0 in
    let isMod4 = \x -> x `mod` 4 == 0 in
    \x -> (isMod4 x) `impl` (isMod2 x)
-}
-- | Another very interesting way to get True, but now accepting any Integral input
third :: forall a. Integral a => a -> Bool
third =
  let
    t1 = (not) :: Bool -> Bool
    t2 = (||) :: Bool -> Bool -> Bool

    impl :: Bool -> Bool -> Bool
    impl x y =
      let
        t3 = t1 x :: Bool
        t4 = t2 t3 :: Bool -> Bool
      in t4 y

    t6 = 0 :: a
    t7 = (mod) :: a -> a -> a
    t8 = (==) :: a -> a -> Bool

    isMod2 :: a -> Bool
    isMod2 x =
      let t9 = 2 :: a
      in t8 (t7 x t9 :: a) t6

    isMod4 :: a -> Bool
    isMod4 x =
      let t10 = 4 :: a
      in t8 (t7 x t10 :: a) t6

  in \x -> impl ((isMod2 x) :: Bool) ((isMod4 x) :: Bool)


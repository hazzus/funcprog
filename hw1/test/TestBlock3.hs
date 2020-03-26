{-# LANGUAGE RankNTypes #-}
module TestBlock3
  ( tests
  ) where

import Block3
import Distribution.TestSuite (Progress (..), Result (..), Test (..), TestInstance (..))

testIt :: (Eq b, Show b) => (a -> b) -> Result -> (a, b) -> Result
testIt f prev (d, e) =
  case prev of
    Pass ->
      if f d == e
      then Pass
      else Fail ("Not matched: " ++ (show $ f d) ++ " /= " ++ show e)
    Fail  _ -> prev
    Error _ -> prev

constructTest :: (Eq b, Show b) => String -> (a -> b) -> [(a, b)] -> Test
constructTest n f results = Test t
  where
    t = TestInstance
      { run = return $ Finished (foldl (testIt f) Pass results)
      , name = n
      , tags = []
      , options = []
      , setOption = \_ _ -> Right t
      }

tests :: IO [Test]
tests = return [testMaybeConcat, testNonEmptySemi, testThisOrThatSemi]

testMaybeConcat :: Test
testMaybeConcat =
  constructTest
    "Testing function maybeConcat"
    maybeConcat
    [ ([Just "aaa", Nothing, Just "bbb", Just "CCC", Nothing], "aaabbbCCC")
    -- , ([Just [1, 2, 3], Nothing, Nothing, Just [4, 5, 6], Just [-1, -2]], [1, 2, 3, 4, 5, 6, -1, -2])
    -- , ([Just [[1, 2], [3]], Nothing, Just [[4]], Nothing, Nothing, Just [[5]]], [[1, 2], [3], [4], [5]])
    , ([Nothing, Nothing], [])
    ]

testNonEmptySemi :: Test
testNonEmptySemi =
  constructTest
    "Testing deriving Semigroup for NonEmpty"
    (uncurry (<>))
    [ ((1 :| [], 2 :| []), 1 :| [2]) ]

testThisOrThatSemi :: Test
testThisOrThatSemi =
  constructTest
    "Testing deriving Semigroup for ThisOrThat"
    (uncurry (<>))
    [ ((This [1 :: Int], That "a"), Both [1 :: Int] "a") ]

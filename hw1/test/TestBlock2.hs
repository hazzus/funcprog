module TestBlock2
    ( tests
    ) where

import Block2
import Data.List.NonEmpty (NonEmpty (..))
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
tests = return [testSplitOn]

testSplitOn :: Test
testSplitOn =
  constructTest
    "Testing splitOn test"
    (uncurry splitOn)
    [ ((' ', "Hello my friend"), "Hello" :| ["my", "friend"])
    , (('/', "path/to/file"), "path" :| ["to", "file"])
    ]

-- TODO property-tests for Foldable

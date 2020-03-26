module TestBlock1
  ( tests
  ) where

import Block1
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
tests = return [task1, task2, task3]

task1 :: Test
task1 = Group
  { groupName = "Test group for Task 1: 'WeekDay'"
  , concurrently = False
  , groupTests =
    [ testEnumConv
    , testNextDay
    , testAfterDays
    , testIsWeekend
    , testDaysToParty
    ]
  }

task2 :: Test
task2 = Group
  { groupName = "Test group for Task 2: 'Nat'"
  , concurrently = False
  , groupTests =
    [ testSum
    , testMul
    , testSub
    , testFromInt
    , testOrd
    ]
  }

task3 :: Test
task3 = Group
  { groupName = "Test group for Task3: 'Tree'"
  , concurrently = False
  , groupTests =
    [ testEmpty
    , testSize
    , testFind
    , testInsert
    , testRemove
    ]
  }

testEnumConv :: Test
testEnumConv =
  constructTest
    "Testing enum deriving"
    toEnum
    [ (0, Monday)
    , (1, Tuesday)
    , (2, Wednesday)
    , (3, Thursday)
    , (4, Friday)
    , (5, Saturday)
    , (6, Sunday)
    ]

testNextDay :: Test
testNextDay =
  constructTest
    "Testing nextDay function"
    nextDay
    [ (Monday   , Tuesday)
    , (Tuesday  , Wednesday)
    , (Wednesday, Thursday)
    , (Thursday , Friday)
    , (Friday   , Saturday)
    , (Saturday , Sunday)
    , (Sunday   , Monday)
    ]

testAfterDays :: Test
testAfterDays =
  constructTest
    "Testing afterDays function"
    (flip (afterDays) 1488)
    [ (Monday   , Friday)
    , (Tuesday  , Saturday)
    , (Wednesday, Sunday)
    , (Thursday , Monday)
    , (Friday   , Tuesday)
    , (Saturday , Wednesday)
    , (Sunday   , Thursday)
    ]

testIsWeekend :: Test
testIsWeekend =
  constructTest
    "Testing isWeekend function"
    isWeekend
    [ (Monday   , False)
    , (Tuesday  , False)
    , (Wednesday, False)
    , (Thursday , False)
    , (Friday   , False)
    , (Saturday , True )
    , (Sunday   , True )
    ]

testDaysToParty :: Test
testDaysToParty =
  constructTest
    "Testing daysToParty function"
    daysToParty
    [ (Monday   , 4)
    , (Tuesday  , 3)
    , (Wednesday, 2)
    , (Thursday , 1)
    , (Friday   , 0)
    , (Saturday , 6)
    , (Sunday   , 5)
    ]

-- Task 2 tests

testSum :: Test
testSum =
  constructTest
    "Testing sum of Nat"
    (uncurry (+))
    [ ((1, Z), 1)
    , ((1, S 2), 4)
    , ((S 42, 42), S 84)
    , ((S 23, S 11), 36)
    ]

testMul :: Test
testMul =
  constructTest
    "Testing mul of Nat"
    (uncurry (*))
    [ ((1, Z), Z)
    , ((1, S 2), 3)
    , ((S 12, 13), S 168)
    , ((S 22, S 10), 253)
    ]

testSub :: Test
testSub =
  constructTest
    "Testing sub of Nat"
    (uncurry (-))
    [ ((1, Z), 1)
    , ((Z, 42), Z)
    , ((1, S 2), Z)
    , ((4, S 2), 1)
    , ((S 12, 1), S 11)
    , ((S 42, S 32), 10)
    ]

testFromInt :: Test
testFromInt =
  constructTest
    "Testing fromInt of Nat"
     fromInteger
    [ (0, Z)
    , (15, S 14)
    , (3, S (S (S Z)))
    , (1, S Z)
    ]

testOrd :: Test
testOrd =
  constructTest
    "Testing deriving Ord"
    (uncurry (<=))
    [ ((Z, S Z), True)
    , ((Z, Z), True)
    , ((S (S Z), S 0), False)
    , ((S (S Z), Z), False)
    , ((S 12, S 5), False)
    , ((S 5, S 42), True)
    ]

-- Task 3 tests

testEmpty :: Test
testEmpty =
  constructTest
    "Testing empty function of Tree"
    empty
    [ (Leaf, True)
    , (Node (1 :| []) Leaf Leaf, False)
    , (Node (1 :| []) (Node (0 :| []) Leaf Leaf) Leaf, False)
    ]

testSize :: Test
testSize =
  constructTest
    "Testing size function of Tree"
    size
    [ (Leaf, 0)
    , (Node (1 :| []) Leaf Leaf, 1)
    , (Node (1 :| [1, 1, 1]) Leaf Leaf, 4)
    , (Node (5 :| [5]) Leaf (Node (7 :| []) (Node (6 :| [6, 6, 6]) Leaf Leaf) Leaf), 7)
    ]

testFind :: Test
testFind =
  constructTest
    "Testing find function of Tree"
    (uncurry find)
    [ ((Leaf, 1), Nothing)
    , ((Node (1 :| []) Leaf Leaf, 1), Just (Node (1 :| []) Leaf Leaf))
    , ((fromList [1, 2, 3, 4, 5, 6], 5), Just (Node (5 :| []) Leaf (Node (6 :| []) Leaf Leaf)))
    ]

testInsert :: Test
testInsert =
  constructTest
    "Testing insert function of Tree"
    (uncurry insert)
    [ ((Leaf, 10), Node (10 :| []) Leaf Leaf)
    , ((fromList [1, 3, 4, 5], -15), fromList [1, 3, 4, 5, -15])
    , ((fromList [10, 30, 5], 20), fromList [10, 30, 5, 20])
    , ((fromList [1, 2, -3], 2), fromList [1, 2, -3, 2])
    ]

testRemove :: Test
testRemove =
  constructTest
    "Testing remove function of Tree"
    (uncurry remove)
    [ ((Leaf, 10), Leaf)
    , ((Node (42 :| []) Leaf Leaf, 42), Leaf)
    , ((fromList [1, 2, 2, -3, 4], 2), fromList [1, 2, -3, 4])
    , ((fromList [10, 5, 15, 20, 17, 1, 8], 5), fromList [10, 8, 1, 15, 20, 17])
    , ((fromList [10, 5, 3], 228), fromList [10, 5, 3])
    ]

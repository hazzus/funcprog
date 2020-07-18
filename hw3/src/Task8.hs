module Task8
  ( SimulationConfig (..)
  , zeroDayGrid
  , tick
  , showGrid
  ) where

import           Control.Comonad (Comonad, duplicate, extend, extract)
import           Control.Monad   (liftM2)
import           Data.List       (intercalate)
import           System.Random   (StdGen, mkStdGen, random, randoms)

data ListZipper a = LZ [a] a [a]

left :: ListZipper a -> ListZipper a
left (LZ (a : as) x bs) = LZ as a (x:bs)
left _                  = error "no way left"

right :: ListZipper a -> ListZipper a
right (LZ as x (b : bs)) = LZ (x:as) b bs
right _                  = error "no way right"

lwrite :: a -> ListZipper a -> ListZipper a
lwrite x (LZ ls _ rs) = LZ ls x rs

toList :: ListZipper a -> Int -> [a]
toList (LZ ls x rs) n = reverse (take n ls) ++ [x] ++ take n rs

instance Functor ListZipper where
  fmap f (LZ ls x rs) = LZ (map f ls) (f x) (map f rs)

iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f

genericMove :: (a -> a) -> (a -> a) -> a -> ListZipper a
genericMove f g e = LZ (iterateTail f e) e (iterateTail g e)


instance Comonad ListZipper where
  extract (LZ _ x _) = x
  duplicate = genericMove left right

newtype Grid a = Grid (ListZipper (ListZipper a))

up :: Grid a -> Grid a
up (Grid g) = Grid $ left g

down :: Grid a -> Grid a
down (Grid g) = Grid $ right g

gleft :: Grid a -> Grid a
gleft (Grid g) = Grid $ fmap left g

gright :: Grid a -> Grid a
gright (Grid g) = Grid $ fmap right g

gwrite :: a -> Grid a -> Grid a
gwrite x (Grid g) = Grid $ lwrite nl g
  where
    nl = lwrite x ol
    ol = extract g

gtoList :: Grid a -> Int -> [[a]]
gtoList (Grid g) n = toList (fmap (flip toList n) g) n

horizontal :: Grid a -> ListZipper (Grid a)
horizontal = genericMove gleft gright

vertical :: Grid a -> ListZipper (Grid a)
vertical = genericMove up down

instance Functor Grid where
  fmap f (Grid g) = Grid $ fmap (fmap f) g

instance Comonad Grid where
  extract (Grid g) = extract $ extract g
  duplicate = Grid . fmap horizontal . vertical

-- Infection functions

data Status
  = Infected
  | Ill
  | Immune
  | Healthy

data Person = Person
  { status  :: Status
  , counter :: Int
  , gen     :: StdGen
  }

instance Show Person where
  show (Person Infected _ _) = "?"
  show (Person Ill _ _)      = "#"
  show (Person Immune _ _)   = "@"
  show _                     = " "

person :: Int -> Person
person = (Person Healthy 0) . mkStdGen

randomList :: Int -> ListZipper Int
randomList i =
  let g = mkStdGen i
      (c, ng) = random g
      rs = randoms ng
  in LZ rs c $ map (0 -) $ rs

randomGrid :: Int -> Grid Int
randomGrid = Grid . (fmap randomList) . randomList

personGrid :: Int -> Grid Person
personGrid x = person <$> randomGrid x

-- | This function creates a zero-day grid -
-- a lot of healthy people with one infected in center
zeroDayGrid :: Int -> Grid Person
zeroDayGrid = createZeroPatient . personGrid

createZeroPatient :: Grid Person -> Grid Person
createZeroPatient g =
  let c = extract g
  in gwrite (c { status = Infected }) g

contacts :: [Grid a -> Grid a]
contacts = horzs ++ verts ++ liftM2 (.) horzs verts
  where
    horzs = [gleft, gright]
    verts = [up, down]

isInfected :: Person -> Bool
isInfected (Person Infected _ _) = True
isInfected (Person Ill _ _)      = True
isInfected _                     = False

infectedCount :: [Person] -> Int
infectedCount = length . filter isInfected

infectedContacts :: Grid Person -> Int
infectedContacts g =
  infectedCount $ map (\dir -> extract $ dir g) contacts

tryInfection :: Float -> StdGen -> Int -> (Bool, StdGen)
tryInfection _   g 0 = (False, g)
tryInfection pos g l =
  let (pos', ng) = random g
      (res, lg) = tryInfection pos ng $ l - 1
  in (pos' < pos || res, lg)

tickPerson :: Float -> Int -> Int -> Int -> Grid Person -> Person
tickPerson pos inf ill imn gr =
  let pers = extract gr
  in case pers of
    Person Ill x g ->
      if x == ill
        then Person Immune 0 g
        else pers { counter = x + 1 }
    Person Infected x g ->
      if x == inf
        then Person Ill 0 g
        else pers { counter = x + 1 }
    Person Immune x g ->
      if x == imn
        then Person Healthy 0 g
        else pers { counter = x + 1 }
    Person Healthy _ g ->
      let (infected, ng) = tryInfection pos g $ infectedContacts gr
      in if infected then Person Infected 0 ng else pers { gen = ng }

-- | Represents a simulation config, used for performing a tick
data SimulationConfig = SimulationConfig
  { possibility :: Float -- ^ possibility of infection while contacting with infected
  , incubate    :: Int -- ^ incubation period duration in ticks
  , illness     :: Int -- ^ illness period duration in ticks
  , immunity    :: Int -- ^ immunity period duration in ticks
  }

-- | Tick performs a tick of simulation with give config
tick :: SimulationConfig -> Grid Person -> Grid Person
tick (SimulationConfig pos inf ill imn) = extend $ tickPerson pos inf ill imn

-- | This function outputs the grid in a beautiful way
showGrid :: Show a => Grid a -> Int -> String
showGrid g n = concat $ intercalate ["\n"] $ gtoList (fmap show g) n

module Main where

import           Control.Monad (foldM)
import           System.Random (newStdGen, random)

import           Task8


main :: IO ()
main = do
  putStrLn "This is infection simulation"
  putStrLn "In next lineis insert please configuration:"
  putStrLn "Grid size: "
  sz <- read <$> getLine
  putStrLn "Duration of simulation: "
  dur <- read <$> getLine
  putStrLn "Possibility of infection:"
  p <- read <$> getLine
  putStrLn "Incubation, Illness and Immunity perios (separated by space): "
  [inc, ill, imn] <- (map read . words) <$> getLine
  let sc = SimulationConfig { possibility = p, incubate = inc, illness = ill, immunity = imn }
  gen <- newStdGen
  let (a, _) = random gen
  lgrid <- foldM (\grid day -> do
    putStrLn $ "Day " ++ show (day :: Int) ++ ":"
    putStrLn $ showGrid grid sz
    return $ tick sc grid
    ) (zeroDayGrid a) [1..dur]
  putStrLn "LAST:"
  putStrLn $ showGrid lgrid sz

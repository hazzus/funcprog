module Main where

import           Control.DeepSeq (NFData)
import           Criterion       (bench, nf)
import           Criterion.Main  (Benchmark, bgroup, defaultMain, env)
import           System.Random   (newStdGen, randomRs)

import           Task1           (Point (..), doubleArea, perimeter)
import qualified Task1Naive      as Naive (doubleArea, perimeter)

randomList :: Int -> IO [Int]
randomList limit = take limit . randomRs (-limit, limit) <$> newStdGen

randomPoints :: Int -> IO [Point]
randomPoints limit = do
  left <- randomList limit
  right <- randomList limit
  return $ zipWith Point left right

fbench :: NFData a => String -> IO [Point] -> ([Point] -> a) -> Benchmark
fbench n d f = env d $ \x ->
  bench n $ nf f x

benches :: NFData a => ([Point] -> a) -> ([Point] -> a) -> Int -> [Benchmark]
benches naive strict limit =
  let points = randomPoints limit
  in
    [ fbench "naive" points naive
    , fbench "strict" points strict
    ]

group :: NFData a => String -> ([Point] -> a) -> ([Point] -> a) -> [Benchmark]
group name naive strict = map toPBench $ take 4 $ iterate (* 100) 10
  where
    toPBench x = bgroup (name ++ "_" ++ show x) $
      benches naive strict x

main :: IO ()
main =
  defaultMain
  $  group "perimeter" Naive.perimeter perimeter
  ++ group "doubleArea" Naive.doubleArea doubleArea

module Sokoban.Benchmarks where

import Criterion.Main (bench, bgroup, defaultMain, whnf)

import qualified StmContainers.Map as StmMap
import Data.Hashable (Hashable)
import GHC.Conc (STM)

interpretStmMapUpdate :: (Hashable k, Eq k) => k -> v -> STM (StmMap.Map k v)
interpretStmMapUpdate k v = do
  m <- StmMap.new
  StmMap.insert v k m
  return m


-- The function we're benchmarking.
--fib :: (Ord a, Num a, Num p) => a -> p
fib :: Integer -> Integer
fib m
  | m < 0 = error "negative!"
  | otherwise = go m
  where
    go 0 = 0
    go 1 = 1
    go n = go (n - 1) + go (n - 2)

-- Our benchmark harness.
benchmark :: IO ()
benchmark =
  defaultMain
    [bgroup "fib"
      [ bench "1" $ whnf fib 1
      , bench "5" $ whnf fib 5
      , bench "9" $ whnf fib 9
      , bench "11" $ whnf fib 11
      ]]

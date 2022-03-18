{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Array.IO (IOArray, newArray, readArray, writeArray, getElems)
import Control.Monad (forM_, when)
import Control.Applicative ((<$>))

--import Sokoban
--
--main :: IO ()
--main = mainS

-- given an array of `n` elements, 0..(n-1), and `k` transitions
-- transition can change i, and (i+1) element and 0 and (n-1) (cycle)
-- find the number of indices of 0 after applying arbitrary `k` transitions.
-- n >= 3

type PArr a = IOArray Int Bool

main :: IO ()
main = do
  nk <- getLine :: IO String
  let [n, k] = map read (words nk)
  arr <- newArray (0, n - 1) False :: IO (PArr Bool)
  temp <- newArray (0, n - 1) False :: IO (PArr Bool)
  writeArray arr 0 True
  forM_ [1 .. k] $ \_ -> performIt n arr temp
  -- calculate the number of true's
  bs <- getElems arr
  print (sum $ map b2i bs)
  where
    b2i :: Bool -> Integer
    b2i b =
      if b
        then 1
        else 0

performIt :: Int -> PArr Bool -> PArr Bool -> IO ()
performIt n arr temp = do
  forM_ [0..n-1] $ \i -> writeArray temp i False
  forM_ [0..n-1] $ \i -> do
    b <- readArray arr i
    when b $ do
      writeArray temp (dec i) True
      writeArray temp (inc i) True
  arr `copyFrom` temp
  where
    dec i = (i - 1 + n) `mod` n
    inc i = (i + 1) `mod` n
    copyFrom a1 a2 =
      forM_ [0..n-1] $ \i -> do
        x <- readArray a2 i
        writeArray a1 i x

getAll :: Read a => IO [a]
getAll = fmap (fmap read . words) getContents

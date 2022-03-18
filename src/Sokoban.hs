{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Sokoban where

import Control.Arrow (second)
import Control.Monad (foldM)
import Data.Monoid   (Sum(..))
import Data.Vector   (Vector)
import Helper        (str)

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C
import qualified Data.FingerTree       as FT
import qualified Data.RBTree           as RB

mainS :: IO ()
mainS = undefined

data Direction
  = U
  | D
  | L
  | R
  deriving (Eq, Show)

data Point = Point
  { x :: Integer
  , y :: Integer
  } deriving (Eq, Show)

data Cell
  = Worker Direction
  | Hole
  | Box
  | Wall
  | Empty
  | BoxOnHole
  | WorkerOnHole Direction
  deriving (Eq, Show)

data Level = Level
  { cells        :: Vector (Vector Cell)
  , charPosition :: Point
  } deriving (Eq, Show)

delta :: Direction -> Point
delta U = Point 0 (-1)
delta D = Point 0 1
delta L = Point (-1) 0
delta R = Point 1 0

parseCell :: Char -> Maybe Cell
parseCell '@' = Just (Worker U)
parseCell '+' = Just (WorkerOnHole U)
parseCell ' ' = Just Empty
parseCell '#' = Just Wall
parseCell '.' = Just Hole
parseCell '$' = Just Box
parseCell '*' = Just BoxOnHole
parseCell _   = Nothing

parseLevel :: B.ByteString -> Maybe Level
parseLevel str
  -- break string into lines
 = do
  let chars = map C.unpack $ C.lines str
  let ijs = [[(i, j) | i <- [0 ..]] | j <- [0 ..]]
  let elems = concat $ zipWith zip ijs chars
  let cells :: [((Integer, Integer), Maybe Cell)]
      cells = map (second parseCell) elems
  Nothing

level :: B.ByteString
level =
  [str|
    #####
   ## . #
   # . $#
  ##$#..#
  #@$ * #
  # $   #
  ###   #
    #####
|]

newtype Elem a =
  Elem a
  deriving (Eq)

instance Show a => Show (Elem a) where
  show (Elem x) = show x

instance FT.Measured (Sum Int) (Elem a) where
  measure (Elem _) = Sum 1

type MyFTree = FT.FingerTree (Sum Int) (Elem Int)

testRB :: IO ()
testRB = do
  let xs = [1 .. 6] :: [Integer]
  ts <- foldM doStuff RB.E xs :: IO (RB.Tree Integer)
  putStrLn $ ">>> final = " ++ show ts
  where
    doStuff t x = do
      let t' = RB.insert x t
      putStrLn $ ">>> insert x t = " ++ show t'
      return t'

testFT :: IO ()
testFT = do
  let ft = mkFt [1 .. 9]
  putStrLn $ ">>> ft=" ++ show ft

mkFt :: [Int] -> MyFTree
mkFt xs =
  let es = map Elem $ reverse xs
  in FT.fromList es

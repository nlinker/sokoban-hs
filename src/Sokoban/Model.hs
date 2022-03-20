{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Sokoban.Model where

import Data.Hashable (Hashable)
import GHC.Generics  (Generic)

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S
import qualified Data.Text           as T

data Direction
  = U
  | D
  | L
  | R
  deriving (Eq, Show)

data Point =
  Point Int Int
  deriving (Eq, Show, Generic, Hashable)

data Cell
  = Worker Direction
  | Hole
  | Box
  | Wall
  | Empty
  | BoxOnHole
  | WorkerOnHole Direction
  deriving (Eq, Show)

data Level =
  Level
    { cells  :: ![[Cell]]
    , height :: !Int
    , width  :: !Int
    , name   :: !T.Text
    }
  deriving (Eq, Show)

data GameState =
  GameState
    { cells      :: M.HashMap Point Cell
    , height     :: !Int
    , width      :: !Int
    , name       :: !T.Text
    , worker     :: !Point
    , workerDrc  :: !Direction
    , boxes      :: S.HashSet Point
    , holes      :: S.HashSet Point
    , isComplete :: !Bool
    }
  deriving (Eq, Show)

initial :: Level -> Maybe GameState
initial level = do
  let m = height (level :: Level)
  let n = width (level :: Level)
  let points = [Point i j | i <- [0 .. m - 1], j <- [0 .. n - 1]]
  let zippedCells = zip points $ concat $ cells (level :: Level)
    -- now extract worker, boxes and holes
    -- worker must exist, and should be 1
    -- the number of boxes should be the number of holes
  let workers = filter (isWorker . snd) zippedCells
  let boxes = filter (isBox . snd) zippedCells
  let holes = filter (isHole . snd) zippedCells
  if length workers /= 1 || length boxes /= length holes
    then Nothing
    else return $
         GameState
           { cells = M.fromList zippedCells
           , height = m
           , width = n
           , name = name (level :: Level)
           , worker = fst $ head workers
           , workerDrc = D
           , boxes = S.fromList $ map fst boxes
           , holes = S.fromList $ map fst holes
           , isComplete = False
           }
  where
    isWorker c =
      case c of
        (Worker _)       -> True
        (WorkerOnHole _) -> True
        _                -> False
    isBox c =
      case c of
        Box       -> True
        BoxOnHole -> True
        _         -> False
    isHole c =
      case c of
        Hole      -> True
        BoxOnHole -> True
        _         -> False

-- We use screen (not Decartes) coordinates (i, j).
-- The origin is in the upper left corner.
delta :: Direction -> Point
delta d =
  case d of
    U -> Point 0 (-1)
    D -> Point 0 1
    L -> Point (-1) 0
    R -> Point 1 0

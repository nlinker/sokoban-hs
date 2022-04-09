{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

module Sokoban.Solver where

import Prelude hiding (Left, Right, id)

import Control.Lens        (use, (%=), (.=), (^.))
import Control.Lens.TH     (makeLenses)
import Control.Monad       (forM_, when)
import Control.Monad.State (StateT, evalStateT, gets, lift)
import Data.Hashable       (Hashable)
import Sokoban.Level       (Direction(..), Point(..), deriveDir)

import qualified Data.HashMap.Strict as H
import qualified Data.HashPSQ        as Q

data Weight p =
  Weight
    { _fScore :: Int
    , _gScore :: Int
    , _parent :: p
    }
  deriving (Eq, Show)

data AStar p =
  AStar
    { _openList   :: Q.HashPSQ p Int (Weight p)
    , _closedList :: H.HashMap p p
    }
  deriving (Eq, Show)

data AStarSolver m p where
  AStarSolver :: (Monad m, Hashable p, Ord p) =>
    { neighbors :: p -> m [p]
    , distance  :: p -> p -> m Int -- for adjacent points only
    , heuristic :: p -> p -> Int
    } -> AStarSolver m p

makeLenses ''Weight
makeLenses ''AStar

aStarFind :: (Monad m, Hashable p, Ord p) => AStarSolver m p -> p -> p -> m [p]
aStarFind solver src dst = do
  let astar = aStarInit src
  evalStateT (aStarFindRec solver dst) astar

aStarInit :: (Hashable p, Ord p) => p -> AStar p
aStarInit src =
  let weight = Weight {_fScore = 0, _gScore = 0, _parent = src}
      openList = Q.singleton src (weight ^. fScore) weight
      closedList = H.empty :: H.HashMap p p
   in AStar openList closedList

aStarFindRec :: (Monad m, Hashable p, Ord p) => AStarSolver m p -> p -> StateT (AStar p) m [p]
aStarFindRec solver dst = do
  openList0 <- use openList
  closedList0 <- use closedList
  case Q.findMin openList0 of
    Nothing -> gets $ backtrace dst <$> flip (^.) closedList
    Just (p0, _, weight0)
      | p0 == dst -> do
        closedList %= H.insert p0 (weight0 ^. parent)
        gets $ backtrace dst <$> flip (^.) closedList
    Just (p0, _, weight0) -> do
      openList %= Q.delete p0
      closedList %= H.insert p0 (weight0 ^. parent)
      neighbors <- lift $ neighbors solver p0
      let neighPoints = filter (not . (`H.member` closedList0)) neighbors
      -- `k` is the current node, `fs` is f-score
      forM_ neighPoints $ \np -> do
        dist <- lift $ distance solver np p0
        let g1 = weight0 ^. gScore + dist
        let f1 = g1 + heuristic solver np dst
        let p1 = p0
        let w1 = Weight {_fScore = f1, _gScore = g1, _parent = p1}
        case Q.lookup np openList0 of
          Just (_, w)
              -- the neighbour can be reached with smaller cost - change priority
              -- otherwise don't touch the neighbour, it will be taken by open_list.pop()
           -> when (g1 < (w ^. gScore)) $ openList .= Q.insert np f1 w1 openList0
          Nothing
            -- the neighbour is new
           -> openList .= Q.insert np f1 w1 openList0
      aStarFindRec solver dst

--      let neighs = filter (not . (`H.member` closedList0)) $ map (movePoint p0) [U, D, L, R]
--      neighbors <- filterM (lift . isAccessible) neighs
backtrace :: (Eq p, Hashable p) => p -> H.HashMap p p -> [p]
backtrace dst closedList
    -- we repeatedly lookup for the parent of the current node
 = backtraceRec dst [dst]
  where
    backtraceRec current acc =
      case H.lookup current closedList of
        Nothing -> []
        Just parent
          | current == parent -> acc
        Just parent -> backtraceRec parent (parent : acc)

pathToDirections :: [Point] -> [Direction]
pathToDirections ps = reverse $ convert ps []
  where
    convert [] _acc = []
    convert [_] acc = acc
    convert (p1:p2:ps) acc =
      case deriveDir p1 p2 of
        Nothing -> acc
        Just d  -> convert (p2 : ps) (d : acc)

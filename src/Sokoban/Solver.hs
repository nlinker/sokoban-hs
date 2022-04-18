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
import Control.Monad.State (StateT, evalStateT, lift, gets)
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
    , heuristic :: p -> p -> m Int
    } -> AStarSolver m p

makeLenses ''Weight
makeLenses ''AStar

aStarFind :: (Monad m, Hashable p, Ord p) => AStarSolver m p -> p -> p -> (p -> m Bool) -> m [p]
aStarFind solver src dst stopCond = do
  let astar = aStarInit src
  evalStateT (aStarFindRec solver dst stopCond) astar

aStarInit :: (Hashable p, Ord p) => p -> AStar p
aStarInit src =
  let weight = Weight {_fScore = 0, _gScore = 0, _parent = src}
      openList = Q.singleton src (weight ^. fScore) weight
      closedList = H.empty :: H.HashMap p p
   in AStar openList closedList

aStarFindRec :: (Monad m, Hashable p, Ord p) => AStarSolver m p -> p -> (p -> m Bool) -> StateT (AStar p) m [p]
aStarFindRec solver dst stopCond = do
  openList0 <- use openList
  closedList0 <- use closedList
  case Q.findMin openList0 of
    Nothing -> return []
    Just (p0, _, weight0) -> do
      finished <- lift $ stopCond p0
      if finished
        then do
          closedList %= H.insert p0 (weight0 ^. parent)
          gets $ backtrace p0 <$> flip (^.) closedList
        else do    
          openList %= Q.delete p0
          closedList %= H.insert p0 (weight0 ^. parent)
          neighbors <- lift $ neighbors solver p0
          let neighPoints = filter (not . (`H.member` closedList0)) neighbors
          -- `k` is the current node, `fs` is f-score
          forM_ neighPoints $ \np -> do
            dist <- lift $ distance solver np p0
            let g1 = weight0 ^. gScore + dist
            hue <- lift $ heuristic solver np dst
            let f1 = g1 + hue
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
          aStarFindRec solver dst stopCond

backtrace :: (Eq p, Hashable p) => p -> H.HashMap p p -> [p]
backtrace dst closedList = backtraceRec dst [dst]
  where
    backtraceRec current acc = -- we repeatedly lookup for the parent of the current node
      case H.lookup current closedList of
        Nothing -> []
        Just parent
          | current == parent -> acc
        Just parent -> backtraceRec parent (parent : acc)

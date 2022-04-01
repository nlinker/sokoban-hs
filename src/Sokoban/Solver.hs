{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

module Sokoban.Solver where

import Prelude hiding (Left, Right, id)

import Control.Lens        (use, (%=), (.=), (^.))
import Control.Lens.TH     (makeLenses)
import Control.Monad       (filterM, forM_, when)
import Control.Monad.State (StateT, evalStateT, gets, lift)
import Sokoban.Level       (Direction(..), Point(..), moveDir)

import qualified Data.HashMap.Strict as H
import qualified Data.HashPSQ        as Q
import Debug.Trace (traceShowM)

data Weight =
  Weight
    { _fScore :: Int
    , _gScore :: Int
    , _parent :: Point
    }
  deriving (Eq, Show)

makeLenses ''Weight

data AStar =
  AStar
    { _openList   :: Q.HashPSQ Point Int Weight
    , _closedList :: H.HashMap Point Point
    }
  deriving (Eq, Show)

makeLenses ''AStar

aStarInit :: Point -> AStar
aStarInit src =
  let weight = Weight {_fScore = 0, _gScore = 0, _parent = src}
      openList = Q.singleton src (weight ^. fScore) weight
      closedList = H.empty :: H.HashMap Point Point
   in AStar openList closedList

aStarFind :: Monad m => Point -> Point -> (Point -> m Bool) -> m [Point]
aStarFind src dst isAccessible = do
  let astar = aStarInit src
  evalStateT (aStarFindRec dst isAccessible) astar

aStarFindRec :: Monad m => Point -> (Point -> m Bool) -> StateT AStar m [Point]
aStarFindRec dst isAccessible = do
  openList0 <- use openList
  closedList0 <- use closedList
  case Q.findMin openList0 of
    Nothing -> gets $ backtrace dst <$> flip (^.) closedList
    Just (p0, _, weight0)
      | p0 == dst -> do
        closedList %= H.insert p0 (weight0 ^. parent)
        cl <- use closedList
        traceShowM cl
        gets $ backtrace dst <$> flip (^.) closedList
    Just (p0, _, weight0) -> do
      openList %= Q.delete p0
      closedList %= H.insert p0 (weight0 ^. parent)
      let neighs = filter (not . (`H.member` closedList0)) $ map (moveDir p0) [U, D, L, R]
      neighbors <- filterM (lift . isAccessible) neighs
      -- `k` is the current node, `fs` is f-score
      forM_ neighbors $ \np -> do
        let g1 = weight0 ^. gScore + fromEnum (np /= p0)
        let f1 = g1 + heuristic np dst
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
      aStarFindRec dst isAccessible

backtrace :: Point -> H.HashMap Point Point -> [Point]
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

heuristic :: Point -> Point -> Int
heuristic next dst =
  let Point i1 j1 = next
      Point i2 j2 = dst
   in abs (i1 - i2) + abs (j1 - j2)

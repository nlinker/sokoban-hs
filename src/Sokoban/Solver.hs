{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module Sokoban.Solver where

import Prelude hiding (Left, Right, id)

import Control.Monad                   (filterM, forM_)
import Control.Monad.Primitive         (PrimMonad(..), PrimState)
import Data.Hashable                   (Hashable)
import Data.Maybe                      (fromMaybe, isJust)
import Text.InterpolatedString.QM      (qm)

import qualified Data.HashMap.Mutable.Basic as HM
import qualified Data.Heap.Mutable.ModelD   as HMD
import Data.Primitive (newMutVar, readMutVar, modifyMutVar', MutVar (MutVar))

newtype Min =
  Min Int
  deriving (Show, Read, Eq, Ord)

instance Semigroup Min where
  (<>) (Min a) (Min b) = Min (min a b)

instance Monoid Min where
  mempty = Min maxBound

data AStarSolver m p where
  AStarSolver :: (Monad m, Hashable p, Eq p) =>
    { neighbors  :: p -> m [p]
    , distance   :: p -> p -> Int   -- for adjacent points only
    , heuristic  :: p -> m Int      -- calculate heuristic distance from first to destination
    , stopCond   :: p -> Bool       -- condition, when the destination is reached   
    , projection :: p -> Int        -- a function to convert points to integer for Heap data structure
    , injection  :: Int -> p        -- an opposite function to projection   
    , nodesBound :: Int             -- upper bound for the number of nodes
    } -> AStarSolver m p


aStarFind :: forall m p . (PrimMonad m, Hashable p, Eq p, Show p) => AStarSolver m p -> p -> m [p]
aStarFind AStarSolver {..} src  = do
  let p2i = projection
  let maxCount = nodesBound
  maxSize <- newMutVar (0 :: Int)
  openHeap <- HMD.new maxCount :: m (HMD.Heap (PrimState m) Min)
  openList <- HM.new           :: m (HM.MHashMap (PrimState m) Int (p, p, Int))
  closedList <- HM.new         :: m (HM.MHashMap (PrimState m) p p)
  let isrc = p2i src
  HMD.unsafePush (Min 0) isrc openHeap
  HM.insert openList isrc (src, src, 0)
  -- return path
  aStarFindRec openHeap openList closedList p2i maxSize
  where
    aStarFindRec
      :: HMD.Heap (PrimState m) Min 
      -> HM.MHashMap (PrimState m) Int (p, p, Int) 
      -> HM.MHashMap (PrimState m) p p
      -> (p -> Int)
      -> MutVar (PrimState m) Int
      -> m [p]
    aStarFindRec openHeap openList closedList p2i maxSize = do
      top' <- HMD.pop openHeap -- remove the minimum and return
      case top' of
          Nothing -> return []
          Just (_fscore, ip0) -> do
            (p0, parent0,  gscore0) <- fromMaybe (error [qm| {ip0} is not found in openList |]) <$> HM.lookup openList ip0
            HM.insert closedList p0 parent0
            if stopCond p0
              then do
                backtraceST closedList p0
              else do
                neighCandidates <- neighbors p0
                let isAcc p = do
                      mc <- member closedList p
                      mo <- member openList (p2i p)
                      return $ not mc && not mo
                neighbors <- filterM isAcc neighCandidates
                forM_ neighbors $ \np -> do
                  let inp = p2i np
                  hue <- heuristic np
                  let dist = distance np p0
                  let gscoreNp = gscore0 + dist
                  let fscoreNp = Min (gscore0 + dist + hue)
                  pg' <- HM.lookup openList inp
                  case pg' of
                    Just (p, parent, gscore) | gscoreNp < gscore -> do
                      -- the neighbour can be reached with smaller cost - change priority
                      -- otherwise don't touch the neighbour, it will be taken by open_list.pop()
                      -- openList .= Q.insert np f1 w1 openList0
                      modifyMutVar' maxSize (+1)
                      HMD.push fscoreNp (p2i p) openHeap
                      HM.insert openList (p2i p) (p, parent, gscoreNp)
                    Nothing -> do
                      -- the neighbour is new
                      -- openList .= Q.insert np f1 w1 openList0
                      modifyMutVar' maxSize (+1)
                      HMD.push fscoreNp inp openHeap
                      HM.insert openList inp (np, p0, gscoreNp)
                    _ -> return ()
                aStarFindRec openHeap openList closedList p2i maxSize

    member :: (PrimMonad m, Hashable k, Eq k) => HM.MHashMap (PrimState m) k a -> k -> m Bool
    member hm p = do
      v' <- HM.lookup hm p
      return $ isJust v'


backtraceST :: forall m p . (PrimMonad m, Eq p, Hashable p, Show p) => HM.MHashMap (PrimState m) p p -> p -> m [p]
backtraceST closedList dst = do
    backtraceRec dst [dst]
  where
    -- we repeatedly lookup for the parent of the current node
    backtraceRec :: (PrimMonad m, Eq p) => p -> [p] -> m [p]
    backtraceRec current acc = do
      parent' <- HM.lookup closedList current
      case parent' of
        Nothing -> return []
        Just parent
          | current == parent -> return acc
        Just parent -> backtraceRec parent (parent : acc)


breadFirstFind :: forall m p . (PrimMonad m, Hashable p, Ord p, Show p) => AStarSolver m p -> p -> m [p]
breadFirstFind AStarSolver{..} src = do
  let p2i = projection
  maxSize <- newMutVar (0 :: Int)
  let maxCount = nodesBound
  path <- do
    openHeap <- HMD.new maxCount :: m (HMD.Heap (PrimState m) Min)
    openList <- HM.new           :: m (HM.MHashMap (PrimState m) Int (p, p))
    closedList <- HM.new         :: m (HM.MHashMap (PrimState m) p p)
    let isrc = p2i src
    HMD.push (Min 0) isrc openHeap
    HM.insert openList isrc (src, src)
    breadFirstFindRec 0
  -- _v <- readMutVar maxSize
  --  traceM [qm| max size = {v} |]
  return path
  where
    keys :: (PrimMonad m, Hashable k, Eq k) => HM.MHashMap (PrimState m) k v -> m [k]
    keys hm = HM.foldM (\a k _v -> return $ k : a) [] hm
    
    member :: (PrimMonad m, Hashable k, Eq k) => HM.MHashMap (PrimState m) k a -> k -> m Bool
    member hm p = do
      v' <- HM.lookup hm p
      return $ isJust v'

    breadFirstFindRec (it :: Int) = do
      -- do looping until heap becomes empty
      top' <- HMD.pop openHeap -- remove the minimum and return
      case top' of
          Nothing -> do
            keys closedList -- gather keys
          Just (Min dist0, ip0) -> do
            (p0, parent0) <- fromMaybe (error [qm| {ip0} is not found in openList |]) <$> HM.lookup openList ip0
            HM.insert closedList p0 parent0
            neighCandidates <- neighbors p0
            let isAcc p = do
                  mc <- member closedList p
                  mo <- member openList (p2i p)
                  return $ not mc && not mo
            neighbors <- filterM isAcc neighCandidates
            forM_ neighbors $ \np -> do
              let inp = p2i np
              let distNp = distance np p0
              let gscoreNp = Min $ dist0 + distNp
              pg' <- HM.lookup openList inp
              case pg' of
                Just (p, parent) -> do
                  -- the neighbour can be reached with smaller cost - change priority
                  -- otherwise don't touch the neighbour, it will be taken by open_list.pop()
                  -- openList .= Q.insert np f1 w1 openList0
                  modifyMutVar' maxSize (+1)
                  HMD.push gscoreNp (p2i p) openHeap
                  HM.insert openList (p2i p) (np, parent)
                Nothing -> do
                  -- the neighbour is new
                  -- openList .= Q.insert np f1 w1 openList0
                  modifyMutVar' maxSize (+1)
                  HMD.push gscoreNp inp openHeap
                  HM.insert openList inp (np, p0)
            breadFirstFindRec (it + 1)

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

module Sokoban.Solver where

import Prelude hiding (Left, Right, id)

import Control.Lens                    (use, (%=), (.=), (^.))
import Control.Lens.TH                 (makeLenses)
import Control.Monad                   (filterM, forM_, when)
import Control.Monad.Primitive         (PrimMonad(..), PrimState)
import Control.Monad.ST.Trans          (runSTT)
import Control.Monad.ST.Trans.Internal (STT(..), STTRet(..))
import Control.Monad.State             (StateT, evalStateT, lift)
import Data.Hashable                   (Hashable)
import Data.Maybe                      (fromMaybe, isJust)
import Text.InterpolatedString.QM      (qm)

import qualified Data.HashMap.Mutable.Basic as HM
import qualified Data.HashMap.Strict        as H
import qualified Data.HashPSQ               as Q
import qualified Data.Heap.Mutable.ModelD   as HPM

data Weight p =
  Weight
    { _fScore :: Int
    , _gScore :: Int
    , _parent :: p
    }
  deriving (Eq, Show)

newtype Min =
  Min Int
  deriving (Show, Read, Eq, Ord)

instance Semigroup Min where
  (<>) (Min a) (Min b) = Min (min a b)

instance Monoid Min where
  mempty = Min maxBound

data AStar s p =
  AStar
    { _heap       :: HPM.Heap s Min
    , _closedList :: HM.MHashMap s p p
--    , _openList   :: Q.HashPSQ p Int (Weight p)
    }

data BreadFirst p =
  BreadFirst
    { _openListB   :: Q.HashPSQ p Int (Weight p)
    , _closedListB :: H.HashMap p ()
    }
  deriving (Eq, Show)

data AStarSolver m p where
  AStarSolver :: (Monad m, Hashable p, Ord p) =>
    { neighbors :: p -> m [p]
    , distance  :: p -> p -> m Int -- for adjacent points only
    , heuristic :: p -> p -> m Int
    , p2i       :: p -> Int
    , i2p       :: Int -> p
    } -> AStarSolver m p


makeLenses ''Weight
makeLenses ''AStar
makeLenses ''BreadFirst

instance (Monad m) => PrimMonad (STT s m) where
  type PrimState (STT s m) = s
  primitive f =
    STT $ \s ->
      case f s of
        (# t, a #) -> return (STTRet t a)
  {-# INLINE primitive #-}

aStarFind :: forall m p . (Monad m, Hashable p, Ord p, Show p) => AStarSolver m p -> p -> p -> (p -> m Bool) -> m [p]
aStarFind solver src dst stopCond = do
  let maxNodesCount = 1000000
  path <- runSTT $ do
    heap <- HPM.new maxNodesCount           :: STT s m (HPM.Heap s Min)
    openList <- HM.new                      :: STT s m (HM.MHashMap s p (p, Int))
    closedList <- HM.newSized maxNodesCount :: STT s m (HM.MHashMap s p p)
    HM.insert openList src (src, 0)
    HPM.unsafePush (mempty :: Min) (p2i solver src) heap
    let aStarFindRec = do
          top' <- HPM.pop heap -- remove the minimum and return
          case top' of
              Nothing -> return []
              Just (_fscore, i) -> do
                let p0 = i2p solver i
                finished <- lift $ stopCond p0
                if finished
                  then do
                    -- HM.insert p0 (weight0 ^. parent) closedList
                    pg' <- HM.lookup openList p0
                    let (parent, _gscore) = fromMaybe (error [qm|{p0} is not found in openList|]) pg'
                    HM.insert closedList p0 parent
                    backtraceST closedList p0
                  else do
                    pg' <- HM.lookup openList p0
                    let (parent, gscore) = fromMaybe (error [qm|{p0} is not found in openList|]) pg'
                    HM.insert closedList p0 parent
                    neighbors <- lift $ neighbors solver p0
                    neighPoints <- filterM (\p -> not <$> member closedList p) neighbors
                    forM_ neighPoints $ \np -> do
                      dist <- lift $ distance solver np p0
                      hue <- lift $ heuristic solver np dst
                      let gscoreNp = gscore + dist
                      let fscoreNp = Min (gscore + dist + hue)
                      pg' <- HM.lookup openList np
                      case pg' of
                        Just (parent, gscore) | gscoreNp < gscore -> do
                          -- the neighbour can be reached with smaller cost - change priority
                          -- otherwise don't touch the neighbour, it will be taken by open_list.pop()
                          -- openList .= Q.insert np f1 w1 openList0
                          HPM.push fscoreNp (p2i solver np) heap
                          HM.insert openList np (parent, gscoreNp)
                        Nothing -> do
                          -- the neighbour is new
                          -- openList .= Q.insert np f1 w1 openList0
                          HPM.push fscoreNp (p2i solver np) heap
                          HM.insert openList np (parent, gscoreNp)
                        _ -> return ()
                    aStarFindRec
    aStarFindRec
  return path
  where
    member :: (Monad m, Hashable k, Eq k) => HM.MHashMap s k a -> k -> STT s m Bool
    member hm p = do
      v' <- HM.lookup hm p
      return $ isJust v'

--aStarFindRec :: (Monad m, Hashable p, Ord p) => AStarSolver m p -> p -> (p -> m Bool) -> StateT (AStar p) m [p]
--aStarFindRec solver dst stopCond = do
--  openList0 <- use openList
--  closedList0 <- use closedList
--  case Q.findMin openList0 of
--    Nothing -> return []
--    Just (p0, _, weight0) -> do
--      finished <- lift $ stopCond p0
--      if finished
--        then do
--          closedList %= H.insert p0 (weight0 ^. parent)
--          gets $ backtrace p0 <$> flip (^.) closedList
--        else do
--          openList %= Q.delete p0
--          closedList %= H.insert p0 (weight0 ^. parent)
--          neighbors <- lift $ neighbors solver p0
--          let neighPoints = filter (not . (`H.member` closedList0)) neighbors
--          -- `k` is the current node, `fs` is f-score
--          forM_ neighPoints $ \np -> do
--            dist <- lift $ distance solver np p0
--            let g1 = weight0 ^. gScore + dist
--            hue <- lift $ heuristic solver np dst
--            let f1 = g1 + hue
--            let p1 = p0
--            let w1 = Weight {_fScore = f1, _gScore = g1, _parent = p1}
--            case Q.lookup np openList0 of
--              Just (_, w)
--                  -- the neighbour can be reached with smaller cost - change priority
--                  -- otherwise don't touch the neighbour, it will be taken by open_list.pop()
--               -> when (g1 < (w ^. gScore)) $ openList .= Q.insert np f1 w1 openList0
--              Nothing
--                -- the neighbour is new
--               -> openList .= Q.insert np f1 w1 openList0
--          aStarFindRec solver dst stopCond


backtraceST :: forall m p . (PrimMonad m, Eq p, Hashable p) => HM.MHashMap (PrimState m) p p -> p -> m [p]
backtraceST closedList dst = backtraceRec dst [dst]
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

aStarInitST :: (PrimMonad m, Ord p) => p -> (p -> Int) -> m (AStar (PrimState m) p)
aStarInitST src p2i = do
  heap <- HPM.new 1000000
  HPM.unsafePush (mempty :: Min) (p2i src) heap
  closedList <- HM.newSized 1000000
  return $ AStar { _heap = heap, _closedList = closedList }

--aStarInit :: (Hashable p, Ord p) => p -> AStar p
--aStarInit src =
--  let weight = Weight {_fScore = 0, _gScore = 0, _parent = src}
--      openList = Q.singleton src (weight ^. fScore) weight
--      closedList = H.empty :: H.HashMap p p
--   in AStar openList closedList

backtrace :: (Eq p, Hashable p) => p -> H.HashMap p p -> [p]
backtrace dst closedList = backtraceRec dst [dst]
  where
    backtraceRec current acc = -- we repeatedly lookup for the parent of the current node
      case H.lookup current closedList of
        Nothing -> []
        Just parent
          | current == parent -> acc
        Just parent -> backtraceRec parent (parent : acc)

breadFirstFind :: (Monad m, Hashable p, Ord p, Show p) => AStarSolver m p -> p -> m [p]
breadFirstFind solver src = do
  let bf = breadFirstInit src
  evalStateT (breadFirstFindRec solver) bf

breadFirstInit :: (Hashable p, Ord p, Show p) => p -> BreadFirst p
breadFirstInit src = do
  let weight = Weight {_fScore = 0, _gScore = 0, _parent = src}
      openList = Q.singleton src (weight ^. fScore) weight
      closedList = H.empty :: H.HashMap p ()
   in BreadFirst openList closedList

breadFirstFindRec :: (Monad m, Hashable p, Ord p, Show p) => AStarSolver m p -> StateT (BreadFirst p) m [p]
breadFirstFindRec solver = do
  openList0 <- use openListB
  closedList0 <- use closedListB
  case Q.findMin openList0 of
    Nothing -> do
      return $ H.keys closedList0
    Just (p0, _, weight0) -> do
          openListB %= Q.delete p0
          closedListB %= H.insert p0 ()
          neighbors <- lift $ neighbors solver p0
          let neighPoints = filter (not . (`H.member` closedList0)) neighbors
          -- `k` is the current node, `fs` is f-score
          forM_ neighPoints $ \np -> do
            dist <- lift $ distance solver np p0
            let g1 = weight0 ^. gScore + dist
            let f1 = g1
            let p1 = p0
            let w1 = Weight {_fScore = f1, _gScore = g1, _parent = p1}
            case Q.lookup np openList0 of
              Just (_, w)
                  -- the neighbour can be reached with smaller cost - change priority
                  -- otherwise don't touch the neighbour, it will be taken by open_list.pop()
               -> when (g1 < (w ^. gScore)) $ openListB .= Q.insert np f1 w1 openList0
              Nothing
                -- the neighbour is new
               -> openListB .= Q.insert np f1 w1 openList0
          breadFirstFindRec solver

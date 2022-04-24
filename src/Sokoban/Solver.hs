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
import qualified Data.Heap.Mutable.ModelD   as HMD
import Debug.Trace (traceM)

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

data BreadFirst p =
  BreadFirst
    { _openListB   :: Q.HashPSQ p Int (Weight p)
    , _closedListB :: H.HashMap p ()
    }
  deriving (Eq, Show)

data AStarSolver m p where
  AStarSolver :: (Monad m, Hashable p, Eq p) =>
    { neighbors  :: p -> m [p]
    , distance   :: p -> p -> Int -- for adjacent points only
    , heuristic  :: p -> p -> m Int
    , projection :: p -> Int
    } -> AStarSolver m p

makeLenses ''Weight
makeLenses ''BreadFirst

instance (Monad m) => PrimMonad (STT s m) where
  type PrimState (STT s m) = s
  primitive f =
    STT $ \s ->
      case f s of
        (# t, a #) -> return (STTRet t a)
  {-# INLINE primitive #-}

aStarFind :: forall m p . (Monad m, Hashable p, Eq p, Show p) => AStarSolver m p -> p -> p -> (p -> m Bool) -> m [p]
aStarFind solver src dst stopCond = do
  let p2i = projection solver
  let maxCount = 1000000
  path <- runSTT $ do
    openHeap <- HMD.new maxCount :: STT s m (HMD.Heap s Min)
    openList <- HM.new           :: STT s m (HM.MHashMap s Int (p, p, Int))
    closedList <- HM.new         :: STT s m (HM.MHashMap s p p)
    let isrc = p2i src
    HMD.unsafePush (Min 0) isrc openHeap
    HM.insert openList isrc (src, src, 0)
    HM.insert closedList src src
    -- the loop until heap becomes empty
    let aStarFindRec = do
          top' <- HMD.pop openHeap -- remove the minimum and return
--          traceM [qm| {top'}|]
--          flip HM.mapM_ openList $ \k v -> do
--            traceM [qm| __ openList: {k} {v}|]
--          flip HM.mapM_ closedList $ \k v -> do
--            traceM [qm| __ closedList: {k} {v}|]
          case top' of
              Nothing -> return []
              Just (_fscore, ip0) -> do
                (p0, parent0,  gscore0) <- fromMaybe (error [qm| {ip0} is not found in openList |]) <$> HM.lookup openList ip0
                HM.insert closedList p0 parent0
                finished <- lift $ stopCond p0
                if finished
                  then do
                    backtraceST closedList p0
                  else do
                    neighCandidates <- lift $ neighbors solver p0
                    let isAcc p = do
                          mc <- member closedList p
                          mo <- member openList (p2i p)
                          return $ not mc && not mo
                    neighbors <- filterM isAcc neighCandidates
                    forM_ neighbors $ \np -> do
                      let inp = p2i np
                      hue <- lift $ heuristic solver np dst
                      let dist = distance solver np p0
                      let gscoreNp = gscore0 + dist
                      let fscoreNp = Min (gscore0 + dist + hue)
                      pg' <- HM.lookup openList inp
                      case pg' of
                        Just (p, parent, gscore) | gscoreNp < gscore -> do
                          -- the neighbour can be reached with smaller cost - change priority
                          -- otherwise don't touch the neighbour, it will be taken by open_list.pop()
                          -- openList .= Q.insert np f1 w1 openList0
                          HMD.push fscoreNp (p2i p) openHeap
                          HM.insert openList (p2i p) (p, parent, gscoreNp)
                        Nothing -> do
                          -- the neighbour is new
                          -- openList .= Q.insert np f1 w1 openList0
                          HMD.push fscoreNp inp openHeap
                          HM.insert openList inp (np, p0, gscoreNp)
                        _ -> return ()
                    aStarFindRec
    aStarFindRec
  return path
  where
    member :: (Monad m, Hashable k, Eq k) => HM.MHashMap s k a -> k -> STT s m Bool
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
            let dist = distance solver np p0
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

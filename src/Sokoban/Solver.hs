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
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE Strict #-}

module Sokoban.Solver where

import Prelude hiding (Left, Right, id)

import Control.Lens            (use, (%=), (.=), (^.))
import Control.Lens.TH         (makeLenses)
import Control.Monad           (forM_, when)
import Control.Monad.State     (StateT, evalStateT, lift, gets)
import Data.Hashable           (Hashable)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST        (ST, runST)

import qualified Data.HashMap.Strict      as H
import qualified Data.HashPSQ             as Q
import qualified Data.Heap.Mutable.ModelD as HeapD
import qualified Data.Map.Strict          as Map
import qualified Data.Set                 as Set
import qualified Data.HashMap.Mutable.Basic as HM

import Data.Monoid   (All (..))
import Data.List     (groupBy)
import Sokoban.Level (Point)
import Data.Word     (Word32)
import Data.Coerce   (coerce)
import Data.Function (on)
import Control.Monad.Identity (runIdentity)
import Debug.Trace (traceShowM, traceM, trace)
import Text.InterpolatedString.QM (qm)

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
  
data AStar s p =
  AStar
    { _heap :: HeapD.Heap s Min  
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
    } -> AStarSolver m p


makeLenses ''Weight
makeLenses ''AStar
makeLenses ''BreadFirst


instance Semigroup Min where
  (<>) (Min a) (Min b) = Min (min a b)

instance Monoid Min where
  mempty = Min maxBound

newtype MyElement = MyElement { getMyElement :: Int }
  deriving (Show,Read,Eq,Ord)


-- λ> :rr heapMatchesList $ (\(f,s) -> (Min f, MyElement s)) <$> [(1,4),(2,3),(3,2),(4,1)]
heapMatchesList :: [(Min,MyElement)] -> Bool
heapMatchesList xs' = runIdentity $ do
  let xs = coerce xs' :: [(Min,Int)]
  let xsSet = fmap (\(p,e) -> (e,p)) xs
  let ys = Map.fromListWith mappend xsSet
  let listRes = Map.toList $ Map.fromListWith Set.union $ map (\(e,p) -> (p,Set.singleton e)) (Map.toList ys)
  let heapRes = runST $ do
        h <- HeapD.new 1000
        HeapD.pushList xs h
        ij0 <- HeapD.pop h
        traceM $ "ij0=" <> show ij0
        HeapD.push (Min 2) 4 h
        ij1 <- HeapD.pop h
        traceM $ "ij1=" <> show ij1
        ij2 <- HeapD.pop h
        traceM $ "ij2=" <> show ij2
        ij3 <- HeapD.pop h
        traceM $ "ij3=" <> show ij3
        ij4 <- HeapD.pop h
        traceM $ "ij4=" <> show ij4
        ijs <- HeapD.popAll h
        return $ ijs
  let heapResSet = map (\pairs@((p,_) : _) -> (p,Set.fromList $ map snd pairs))
        $ groupBy (on (==) fst) heapRes
  return $ trace [qm| xs={xs}\nheapRes={heapRes}|] $ heapResSet == listRes

aStarFind :: (Monad m, Hashable p, Ord p) => AStarSolver m p -> p -> p -> (p -> m Bool) -> (p -> Int) -> (Int -> p) -> m [p]
aStarFind solver src dst stopCond p2i i2p = do
  let path = []
  path <- runST $ do
    ast <- aStarInitST src p2i
    undefined
  return path
  where

aStarInitST :: (PrimMonad m, Ord p) => p -> (p -> Int) -> m (AStar (PrimState m) p)
aStarInitST src p2i = do 
  heap <- HeapD.new 1000000 
  HeapD.unsafePush (mempty :: Min) (p2i src) heap
  closedList <- HM.newSized 1000000
  return $ AStar { _heap = heap, _closedList = closedList }
--    f2p k = Point (k `div` n) (k `mod` n)

aStarFindST :: (PrimMonad m, Hashable p, Ord p) => m [p]
aStarFindST = undefined

--aStarInit :: (Hashable p, Ord p) => p -> AStar p
--aStarInit src =
--  let weight = Weight {_fScore = 0, _gScore = 0, _parent = src}
--      openList = Q.singleton src (weight ^. fScore) weight
--      closedList = H.empty :: H.HashMap p p
--   in AStar openList closedList

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

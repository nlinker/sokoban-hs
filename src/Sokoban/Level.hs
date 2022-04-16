{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

module Sokoban.Level where

import Control.Lens                ((&), (+~), _1, _2)
import Control.Lens.TH             (makeLenses, makePrisms)
import Control.Monad               (liftM)
import Data.Hashable               (Hashable)
import Data.Vector.Unboxed.Base    (MVector(..), Vector(..))
import Data.Vector.Unboxed.Mutable (Unbox)
import Data.Word                   (Word8)
import GHC.Generics                (Generic)

import qualified Data.Text                   as T
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Primitive       as P

data Direction
  = U
  | D
  | L
  | R
  deriving (Eq, Ord, Show, Generic, Hashable)

data Cell
  = Worker {-# UNPACK #-}!Direction
  | WorkerOnGoal {-# UNPACK #-}!Direction
  | Goal
  | Box
  | BoxOnGoal
  | Empty
  | Wall
  deriving (Eq, Show)

data Level =
  Level
    { _cells  :: ![[Cell]]
    , _height :: !Int
    , _width  :: !Int
    , _id     :: !T.Text
    }
  deriving (Eq, Show)

-- Fields are took from SLC format, but more flattened
-- e.g. http://www.sourcecode.se/sokoban/download/microban.slc
data LevelCollection =
  LevelCollection
    { _title       :: !T.Text
    , _description :: !T.Text
    , _email       :: !T.Text
    , _url         :: !T.Text
    , _copyright   :: !T.Text
    , _levels      :: ![Level]
    }
  deriving (Eq, Show)

data Point =
  Point Int Int
  deriving (Eq, Show, Generic, Hashable)

instance Ord Point where
  compare (Point i1 j1) (Point i2 j2) = compare (i1, j1) (i2, j2)

data PD =
  PD Point Direction
  deriving (Eq, Show, Generic, Hashable)

instance Ord PD where
  compare (PD p1 d1) (PD p2 d2) = compare (p1, d1) (p2, d2)

makeLenses ''Level

makeLenses ''LevelCollection

makePrisms ''Point

makePrisms ''PD

-- We use screen (not Decartes) coordinates (i, j).
-- The origin is in the upper left corner.
movePoint :: Point -> Direction -> Point
movePoint p d =
  case d of
    U -> p & _Point . _1 +~ -1
    D -> p & _Point . _1 +~ 1
    L -> p & _Point . _2 +~ -1
    R -> p & _Point . _2 +~ 1

opposite :: Direction -> Direction
opposite d =
  case d of
    U -> D
    D -> U
    L -> R
    R -> L

deriveDir :: Point -> Point -> Maybe Direction
deriveDir (Point i1 j1) (Point i2 j2) =
  case (i2 - i1, j2 - j1) of
    (-1, 0) -> Just U
    (1, 0)  -> Just D
    (0, -1) -> Just L
    (0, 1)  -> Just R
    _       -> Nothing

isWorker :: Cell -> Bool
isWorker c =
  case c of
    (Worker _)       -> True
    (WorkerOnGoal _) -> True
    _                -> False

isBox :: Cell -> Bool
isBox c =
  case c of
    Box       -> True
    BoxOnGoal -> True
    _         -> False

isEmptyOrGoal :: Cell -> Bool
isEmptyOrGoal c =
  case c of
    Empty -> True
    Goal  -> True
    _     -> False

isGoal :: Cell -> Bool
isGoal c =
  case c of
    Goal           -> True
    BoxOnGoal      -> True
    WorkerOnGoal _ -> True
    _              -> False

isWall :: Cell -> Bool
isWall c = c == Wall

------------------------------------------------
-- unboxed representation for Vector of Cells --
------------------------------------------------
fromDirection :: Direction -> Word8
{-# INLINE fromDirection #-}
fromDirection d =
  case d of
    U -> 0
    D -> 1
    L -> 2
    R -> 3

toDirection :: Word8 -> Direction
{-# INLINE toDirection #-}
toDirection w =
  case w of
    0 -> U
    1 -> D
    2 -> L
    _ -> R

fromCell :: Cell -> Word8
{-# INLINE fromCell #-}
fromCell c =
  case c of
    Empty          -> 0 * 8 + 0
    Box            -> 0 * 8 + 1
    Worker d       -> 0 * 8 + 4 + fromDirection d
    Goal           -> 1 * 8 + 0
    BoxOnGoal      -> 1 * 8 + 1
    WorkerOnGoal d -> 1 * 8 + 4 + fromDirection d
    Wall           -> 2 * 8 + 0

toCell :: Word8 -> Cell
{-# INLINE toCell #-}
toCell w =
  let stable = w `div` 8
      movable = w `mod` 8
   in case (stable, movable) of
        (0, 0) -> Empty
        (0, 1) -> Box
        (0, d) -> Worker (toDirection (d - 4))
        (1, 0) -> Goal
        (1, 1) -> BoxOnGoal
        (1, d) -> WorkerOnGoal (toDirection (d - 4))
        _      -> Wall

newtype instance  MVector s Cell = MV_Cell (P.MVector s Word8)

newtype instance  Vector Cell = V_Cell (P.Vector Word8)

instance Unbox Cell

instance M.MVector MVector Cell where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Cell v) = M.basicLength v
  basicUnsafeSlice i n (MV_Cell v) = MV_Cell $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Cell v1) (MV_Cell v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Cell `liftM` M.basicUnsafeNew n
  basicInitialize (MV_Cell v) = M.basicInitialize v
  basicUnsafeReplicate n x = MV_Cell `liftM` M.basicUnsafeReplicate n (fromCell x)
  basicUnsafeRead (MV_Cell v) i = toCell `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Cell v) i x = M.basicUnsafeWrite v i (fromCell x)
  basicClear (MV_Cell v) = M.basicClear v
  basicSet (MV_Cell v) x = M.basicSet v (fromCell x)
  basicUnsafeCopy (MV_Cell v1) (MV_Cell v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Cell v1) (MV_Cell v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Cell v) n = MV_Cell `liftM` M.basicUnsafeGrow v n

instance G.Vector Vector Cell where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Cell v) = V_Cell `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Cell v) = MV_Cell `liftM` G.basicUnsafeThaw v
  basicLength (V_Cell v) = G.basicLength v
  basicUnsafeSlice i n (V_Cell v) = V_Cell $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Cell v) i = toCell `liftM` G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Cell mv) (V_Cell v) = G.basicUnsafeCopy mv v
  elemseq _ = seq

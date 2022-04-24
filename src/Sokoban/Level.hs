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

import Control.Lens                ((^.))
import Control.Lens.TH             (makeLenses, makePrisms)
import Control.Monad               (liftM)
import Data.Hashable               (Hashable)
import Data.Vector.Unboxed.Base    (MVector(..), Vector(..))
import Data.Vector.Unboxed.Mutable (Unbox)
import Data.Word                   (Word8)
import GHC.Generics                (Generic)

import           Control.Arrow               ((&&&))
import           Data.Function               (on)
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

-- directed point, we can use this for boxes
data PD =
  PD Point Direction [Direction]
  deriving (Eq, Generic, Hashable)

-- double directed point, we can use this for box moves
data PPDD =
  PPDD
    { _pointFst   :: Point
    , _pointSnd   :: Point
    , _dirFst     :: Direction
    , _dirSnd     :: Direction
    , _directions :: [Direction]
    }
  deriving (Eq, Generic, Hashable)

makeLenses ''Level

makeLenses ''LevelCollection

makePrisms ''Point

makePrisms ''PD

makeLenses ''PPDD

instance Show PD where
  show (PD (Point i j) d dirs) = "(" <> show i <> " " <> show j <> " " <> show d <> " " <> show dirs <> ")"

instance Ord PD where
  compare (PD p1 d1 ds1) (PD p2 d2 ds2) = compare (p1, d1, ds1) (p2, d2, ds2)

instance Show PPDD where
  show pd =
    "(" <> show (pd ^. pointFst) <> " " <> show (pd ^. pointSnd) <> " " <> show (pd ^. dirFst) <> " " <>
    show (pd ^. dirSnd) <>
    " " <>
    show (pd ^. directions) <>
    ")"

instance Ord PPDD where
  compare = compare `on` (_pointFst &&& _pointSnd &&& _dirFst &&& _dirSnd)
  -- elegant variation for
  --   compare pd1 pd2 = compare
  --     (pd1 ^. pointFst, pd1 ^. pointSnd, pd1 ^. dirFst, pd1 ^. dirSnd)
  --     (pd2 ^. pointFst, pd2 ^. pointSnd, pd2 ^. dirFst, pd2 ^. dirSnd)

-- We use screen (not Decartes) coordinates (i, j).
-- The origin is in the upper left corner.
movePoint :: Point -> Direction -> Point
movePoint (Point i j) d =
  case d of
    U -> Point (i - 1) j
    D -> Point (i + 1) j
    L -> Point i (j - 1)
    R -> Point i (j + 1)

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
w8FromDirection :: Direction -> Word8
{-# INLINE w8FromDirection #-}
w8FromDirection d =
  case d of
    U -> 0
    D -> 1
    L -> 2
    R -> 3

w8ToDirection :: Word8 -> Direction
{-# INLINE w8ToDirection #-}
w8ToDirection w =
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
    Worker d       -> 0 * 8 + 4 + w8FromDirection d
    Goal           -> 1 * 8 + 0
    BoxOnGoal      -> 1 * 8 + 1
    WorkerOnGoal d -> 1 * 8 + 4 + w8FromDirection d
    Wall           -> 2 * 8 + 0

toCell :: Word8 -> Cell
{-# INLINE toCell #-}
toCell w =
  let stable = w `div` 8
      movable = w `mod` 8
   in case (stable, movable) of
        (0, 0) -> Empty
        (0, 1) -> Box
        (0, d) -> Worker (w8ToDirection (d - 4))
        (1, 0) -> Goal
        (1, 1) -> BoxOnGoal
        (1, d) -> WorkerOnGoal (w8ToDirection (d - 4))
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

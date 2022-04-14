{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

module Sokoban.Level where

import Control.Lens    ((&), (+~), _1, _2)
import Control.Lens.TH (makeLenses, makePrisms)
import Data.Hashable   (Hashable)
import GHC.Generics    (Generic)

import qualified Data.Text as T

data Direction
  = U
  | D
  | L
  | R
  deriving (Eq, Show)

data Cell
  = Worker Direction
  | WorkerOnGoal Direction
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

makeLenses ''Level

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

makeLenses ''LevelCollection

data Point =
  Point Int Int
  deriving (Eq, Show, Generic, Hashable)

instance Ord Point where
  (Point i1 j1) <= (Point i2 j2) = (i1 <= i2) || ((i1 < i2) && (j1 <= j2))

makePrisms ''Point

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

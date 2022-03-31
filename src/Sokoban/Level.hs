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
moveDir :: Point -> Direction -> Point
moveDir p d =
  case d of
    U -> p & _Point . _1 +~ -1
    D -> p & _Point . _1 +~ 1
    L -> p & _Point . _2 +~ -1
    R -> p & _Point . _2 +~ 1

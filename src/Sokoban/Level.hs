{-# LANGUAGE TemplateHaskell #-}

module Sokoban.Level where

import Control.Lens.TH (makeLenses)

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

{-# LANGUAGE TemplateHaskell #-}

module Sokoban.Level where

import Lens.Micro.TH (makeLenses)

import qualified Data.Text as T

data Direction
  = U
  | D
  | L
  | R
  deriving (Eq, Show)

data Cell
  = Worker Direction
  | Hole
  | Box
  | Wall
  | Empty
  | BoxOnHole
  | WorkerOnHole Direction
  deriving (Eq, Show)

data Level =
  Level
    { _cells  :: ![[Cell]]
    , _height :: !Int
    , _width  :: !Int
    , _name   :: !T.Text
    }
  deriving (Eq, Show)

makeLenses ''Level

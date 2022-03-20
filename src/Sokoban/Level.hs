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
  | WorkerOnHole Direction
  | Hole
  | Box
  | BoxOnHole
  | Empty
  | Wall
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

{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

module Sokoban.Solver where

--import           Prelude hiding (Left, Right, id)
--import qualified Prelude as P
--
--import Control.Arrow       (second)
--import Control.Lens        (Lens', ix, lens, use, (%=), (&), (+~), (.=), (.~), (<>=), (^.), _1, _2,
--                            _3)
--import Control.Lens.TH     (makeLenses, makePrisms)
--import Control.Monad       (filterM, forM_, unless, when)
--import Control.Monad.State (MonadState, evalState, execState)
--import Data.Hashable       (Hashable)
--import Data.Vector         (Vector, (!))
--import GHC.Generics        (Generic)
--import Sokoban.Level       (Cell(..), Direction(..), Level, LevelCollection, levels)
--import Sokoban.Model       (GameState, Point(..), getCell, isEmptyOrGoal, moveDir)
--
--import qualified Data.HashMap.Strict as H
--import qualified Data.HashPSQ        as Q
--import qualified Data.HashSet        as S
--import qualified Data.Text           as T
--import qualified Data.Vector         as V
--import qualified Sokoban.Level       as L (cells, height, id, width)
--import qualified Text.Builder        as TB

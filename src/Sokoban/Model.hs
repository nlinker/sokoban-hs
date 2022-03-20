{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Sokoban.Model where

import Prelude hiding (Left, Right)

import Control.Monad.Identity (runIdentity)
import Control.Monad.State    (execState, modify)
import Data.Hashable          (Hashable)
import Data.Vector            (Vector, (!))
import GHC.Generics           (Generic)
import Lens.Micro             ((&), (.~), (^.))
import Lens.Micro.TH          (makeLenses)
import Sokoban.Level          (Cell(..), Direction(..), Level)

import qualified Data.HashSet        as S
import qualified Data.Text           as T
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as W
import qualified Sokoban.Level       as L

data Point =
  Point Int Int
  deriving (Eq, Show, Generic, Hashable)

type MatrixCell = Vector (Vector Cell)

data GameState =
  GameState
    { _cells      :: MatrixCell
    , _height     :: !Int
    , _width      :: !Int
    , _name       :: !T.Text
    , _worker     :: !Point
    , _workerDrc  :: !Direction
    , _boxes      :: S.HashSet Point
    , _holes      :: S.HashSet Point
    , _isComplete :: !Bool
    }
  deriving (Eq, Show)

data Action
  = Up
  | Down
  | Left
  | Right
  | Undo
  | Restart
  | MoveBoxStart Point
  | MoveBoxEnd Point
  | MoveWorker Point

makeLenses ''GameState

initial :: Level -> Maybe GameState
initial level = do
  let m = level ^. L.height
  let n = level ^. L.width
  let points = [Point i j | i <- [0 .. m - 1], j <- [0 .. n - 1]]
  let levelCells = level ^. L.cells
  let zippedCells = zip points $ concat levelCells
    -- now extract worker, boxes and holes
    -- worker must exist, and should be 1
    -- the number of boxes should be the number of holes
  let workers = filter (isWorker . snd) zippedCells
  let boxes = filter (isBox . snd) zippedCells
  let holes = filter (isHole . snd) zippedCells
  if length workers /= 1 || length boxes /= length holes
    then Nothing
    else return $
         GameState
           { _cells = V.fromList $ V.fromList <$> levelCells
           , _height = m
           , _width = n
           , _name = level ^. L.name
           , _worker = fst $ head workers
           , _workerDrc = D
           , _boxes = S.fromList $ map fst boxes
           , _holes = S.fromList $ map fst holes
           , _isComplete = False
           }
  where
    isWorker c =
      case c of
        (Worker _)       -> True
        (WorkerOnHole _) -> True
        _                -> False
    isBox c =
      case c of
        Box       -> True
        BoxOnHole -> True
        _         -> False
    isHole c =
      case c of
        Hole      -> True
        BoxOnHole -> True
        _         -> False

step :: GameState -> Action -> GameState
step gameState action =
  flip execState gameState $ do
    modify testModify
    return ()
  where
    testModify :: GameState -> GameState
    testModify gs =
      runIdentity $ do
        let wdrc = gs ^. workerDrc
        let wp = gs ^. worker
        let Point wi wj = wp
        let (wp1, wdrc1) =
              case action of
                Up    -> (Point (wi - 1) wj, U)
                Down  -> (Point (wi + 1) wj, D)
                Left  -> (Point wi (wj - 1), L)
                Right -> (Point wi (wj + 1), R)
                _     -> (wp, wdrc)
        let cs = updateCell (gs ^. cells) wp Empty
        let cs2 =
              if S.member wp1 $ gs ^. holes
                then updateCell cs wp1 (WorkerOnHole wdrc1)
                else updateCell cs wp1 (Worker wdrc1)
        return $ gs & cells .~ cs2 & worker .~ wp1 & workerDrc .~ wdrc1

-- We use screen (not Decartes) coordinates (i, j).
-- The origin is in the upper left corner.
delta :: Direction -> Point
delta d =
  case d of
    U -> Point 0 (-1)
    D -> Point 0 1
    L -> Point (-1) 0
    R -> Point 1 0

getCell :: MatrixCell -> Point -> Cell
getCell mtx p =
  let Point i j = p
   in (mtx ! i) ! j

updateCell :: MatrixCell -> Point -> Cell -> MatrixCell
updateCell mtx p cell =
  let Point i j = p
   in let row = V.modify (\v -> W.write v j cell) (mtx ! i)
       in V.modify (\vv -> W.write vv i row) mtx

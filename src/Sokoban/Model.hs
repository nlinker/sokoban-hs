{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE TupleSections         #-}

module Sokoban.Model where

import Prelude hiding (Left, Right)

import Control.Lens        (ix, use, (%=), (&), (+~), (.=), (.~), (^.), _1, _2, _3)
import Control.Lens.TH     (makeLenses, makePrisms)
import Control.Monad       (forM_, when)
import Control.Monad.State (State, execState)
import Data.Hashable       (Hashable)
import Data.Vector         (Vector, (!))
import GHC.Generics        (Generic)
import Sokoban.Level       (Cell(..), Direction(..), Level)

import qualified Data.HashSet  as S
import qualified Data.Text     as T
import qualified Data.Vector   as V
import           Debug.Trace   (trace)
import qualified Sokoban.Level as L (cells, height, name, width)

data Point =
  Point Int Int
  deriving (Eq, Show, Generic, Hashable)

makePrisms ''Point

type MatrixCell = Vector (Vector Cell)

data Diff =
  Diff
    { _point     :: Point
    , _direction :: Direction
    , _cell0     :: Cell
    , _cell1     :: Cell
    , _cell2     :: Cell
    }
  deriving (Eq, Show)

makeLenses ''Diff

data GameState =
  GameState
    { _cells      :: MatrixCell
    , _origin     :: MatrixCell
    , _height     :: !Int
    , _width      :: !Int
    , _name       :: !T.Text
    , _worker     :: !Point
    , _boxes      :: S.HashSet Point
    , _holes      :: S.HashSet Point
    , _isComplete :: !Bool
    , _undoStack  :: [Diff]
    }
  deriving (Eq, Show)

makeLenses ''GameState

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
  deriving (Eq, Show)

initial :: Level -> Maybe GameState
initial level = do
  let m = level ^. L.height
  let n = level ^. L.width
  let levelCells = level ^. L.cells
  let cells = V.fromList $ V.fromList <$> levelCells
  case extractWBH cells of
    Nothing -> Nothing
    Just (worker, boxes, holes) -> do
      return $
        GameState
          { _cells = cells
          , _origin = cells
          , _height = m
          , _width = n
          , _name = level ^. L.name
          , _worker = worker
          , _boxes = boxes
          , _holes = holes
          , _isComplete = False
          , _undoStack = []
          }

-- extract worker, boxes and holes, needed to be run after start, restart or undo
extractWBH :: MatrixCell -> Maybe (Point, S.HashSet Point, S.HashSet Point)
extractWBH xs =
  let (workers, boxes, holes) = (flip execState) ([], [], []) extract
      nws = length workers
      nbs = length boxes
      nhs = length holes
   in (trace $ "nws nbs nhs = " <> show (nws, nbs, nhs)) $
      if nws == 1 && nbs > 0 && nbs == nhs
        then Just (head workers, S.fromList boxes, S.fromList holes)
        else Nothing
  where
    extract = do
      let m = V.length xs
      let n = V.length (xs ! 0)
      forM_ [0 .. m - 1] $ \i ->
        forM_ [0 .. n - 1] $ \j -> do
          let x = (xs ! i) ! j
          when (isWorker x) $ _1 %= ((Point i j) :)
          when (isBox x) $ _2 %= ((Point i j) :)
          when (isHole x) $ _3 %= ((Point i j) :)
      return ()

isWorker :: Cell -> Bool
isWorker c =
  case c of
    (Worker _)       -> True
    (WorkerOnHole _) -> True
    _                -> False

isBox :: Cell -> Bool
isBox c =
  case c of
    Box       -> True
    BoxOnHole -> True
    _         -> False

isEmptyOrHole :: Cell -> Bool
isEmptyOrHole c =
  case c of
    Empty -> True
    Hole  -> True
    _     -> False

isHole :: Cell -> Bool
isHole c =
  case c of
    Hole           -> True
    BoxOnHole      -> True
    WorkerOnHole _ -> True
    _              -> False

step :: GameState -> Action -> GameState
step gameState action = flip execState gameState $ runStep action

runStep :: Action -> State GameState ()
runStep action = do
  case action of
    Up      -> moveWorker $ toDirection action
    Down    -> moveWorker $ toDirection action
    Left    -> moveWorker $ toDirection action
    Right   -> moveWorker $ toDirection action
    Restart -> restartLevel
    Undo    -> undoMove
    _       -> return ()
    -- now compare the sets and check the game completion
  hs <- use holes
  bs <- use boxes
  when (hs == bs) $ isComplete .= True
  where
    restartLevel :: State GameState ()
    restartLevel = do
      originCells <- use origin
      case extractWBH originCells of
        Nothing -> error $ "Invariant violation: " <> show originCells
        Just (w, b, h) -> do
          worker .= w
          boxes .= b
          holes .= h
          cells .= originCells
          undoStack .= []
      return ()
    undoMove :: State GameState ()
    undoMove = do
      diffs <- use undoStack
      case diffs of
        [] -> return ()
        (diff:diffs) -> do
          let point0 = diff ^. point
          let point1 = moveDir point0 $ diff ^. direction
          let point2 = moveDir point1 $ diff ^. direction
          updateCell point0 $ diff ^. cell0
          updateCell point1 $ diff ^. cell1
          updateCell point2 $ diff ^. cell2
          undoStack .= diffs
      cells <- use cells
      case extractWBH cells of
        Nothing -> error $ "Invariant violation: " <> show cells
        Just (w, b, h) -> do
          worker .= w
          boxes .= b
          holes .= h
      return ()
    moveWorker :: Direction -> State GameState ()
    moveWorker d = do
      point0 <- use worker
      let point1 = moveDir point0 d
      let point2 = moveDir point1 d
      c0' <- getCell point0
      c1 <- getCell point1
      c2 <- getCell point2
      let c0 = directWorker d c0'
      -- cells c0 can differ from c0' in direction only, and must be Worker / WorkerOnHole cell
      let ((d0, d1, d2), moveStatus) = move (c0, c1, c2)
      -- this variant makes redirections also to be recorded and undoable
      -- > let diff = Diff {_point = point0, _direction = d, _cell0 = c0, _cell1 = c1, _cell2 = c2}
      -- > when ((d0, d1, d2) /= (c0, c1, c2)) $ undoStack %= (diff :)
      let diff = Diff {_point = point0, _direction = d, _cell0 = c0, _cell1 = c1, _cell2 = c2}
      when ((d0, d1, d2) /= (c0, c1, c2)) $ undoStack %= (diff :)
      case moveStatus of
        Just True
          -- moved both worker and box
         -> do
          boxes %= (S.insert point2 . S.delete point1)
          worker .= point1
          updateCell point0 d0
          updateCell point1 d1
          updateCell point2 d2
        Just False
          -- moved worker only
         -> do
          worker .= point1
          updateCell point0 d0
          updateCell point1 d1
        Nothing
          -- nothing moved (but the worker could change the direction)
         -> do
          updateCell point0 d0
      --      let msg1 = "c012 = " <> show (c0, c1, c2) <> " -- d012 = " <> show (d0, d1, d2) <> "          \n"
      --      name .= (T.pack $ msg1)
      return ()
    toDirection :: Action -> Direction
    toDirection a =
      case a of
        Up    -> U
        Down  -> D
        Left  -> L
        Right -> R
        _     -> error $ "Should not happen: " <> show a
    directWorker d cw =
      case cw of
        Worker _       -> Worker d
        WorkerOnHole _ -> WorkerOnHole d
        cell           -> cell

move :: (Cell, Cell, Cell) -> ((Cell, Cell, Cell), Maybe Bool)
move triple =
  case triple of
    (Worker d, Box, Empty)             -> ((Empty, Worker d, Box), Just True)
    (Worker d, Box, Hole)              -> ((Empty, Worker d, BoxOnHole), Just True)
    (Worker d, BoxOnHole, Empty)       -> ((Empty, WorkerOnHole d, Box), Just True)
    (Worker d, BoxOnHole, Hole)        -> ((Empty, WorkerOnHole d, BoxOnHole), Just True)
    (WorkerOnHole d, Box, Empty)       -> ((Hole, Worker d, Box), Just True)
    (WorkerOnHole d, Box, Hole)        -> ((Hole, Worker d, BoxOnHole), Just True)
    (WorkerOnHole d, BoxOnHole, Empty) -> ((Hole, WorkerOnHole d, Box), Just True)
    (WorkerOnHole d, BoxOnHole, Hole)  -> ((Hole, WorkerOnHole d, BoxOnHole), Just True)
    (Worker d, Empty, c3)              -> ((Empty, Worker d, c3), Just False)
    (Worker d, Hole, c3)               -> ((Empty, WorkerOnHole d, c3), Just False)
    (WorkerOnHole d, Empty, c3)        -> ((Hole, Worker d, c3), Just False)
    (WorkerOnHole d, Hole, c3)         -> ((Hole, WorkerOnHole d, c3), Just False)
    _                                  -> (triple, Nothing)

-- We use screen (not Decartes) coordinates (i, j).
-- The origin is in the upper left corner.
moveDir :: Point -> Direction -> Point
moveDir p d =
  case d of
    U -> p & _Point . _1 +~ -1
    D -> p & _Point . _1 +~ 1
    L -> p & _Point . _2 +~ -1
    R -> p & _Point . _2 +~ 1

getCell :: Point -> State GameState Cell
getCell p = do
  cs <- use cells
  m <- use height
  n <- use width
  let Point i j = p
  if 0 <= i && i < m && 0 <= j && j < n
    then return $ (cs ! i) ! j
    else return Wall

updateCell :: Point -> Cell -> State GameState ()
updateCell p cell = do
  cs <- use cells
  m <- use height
  n <- use width
  -- awesome lenses to access 2d vector
  --   in let row = V.modify (\v -> W.write v j cell) (mtx ! i)
  --       in V.modify (\vv -> W.write vv i row) mtx
  let Point i j = p
  if 0 <= i && i < m && 0 <= j && j < n
    then cells .= (cs & ix i . ix j .~ cell)
    else return ()

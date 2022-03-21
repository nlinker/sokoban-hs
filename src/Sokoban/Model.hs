{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE TupleSections         #-}

module Sokoban.Model where

import Prelude hiding (Left, Right)

import Control.Lens        (ix, use, (&), (+~), (.=), (.~), (^.), _1, _2)
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
import qualified Sokoban.Level as L (cells, height, name, width)

data Point =
  Point Int Int
  deriving (Eq, Show, Generic, Hashable)

makePrisms ''Point

type MatrixCell = Vector (Vector Cell)

data GameState =
  GameState
    { _cells      :: MatrixCell
    , _height     :: !Int
    , _width      :: !Int
    , _name       :: !T.Text
    , _worker     :: !Point
    , _boxes      :: S.HashSet Point
    , _holes      :: S.HashSet Point
    , _isComplete :: !Bool
    , _undoStack  :: ![MatrixCell]
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
  deriving (Eq, Show)

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
    else do
      let cells = V.fromList $ V.fromList <$> levelCells
      return $
        GameState
          { _cells = cells
          , _height = m
          , _width = n
          , _name = level ^. L.name
          , _worker = fst $ head workers
          , _boxes = S.fromList $ map fst boxes
          , _holes = S.fromList $ map fst holes
          , _isComplete = False
          , _undoStack = [cells]
          }

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
    Hole      -> True
    BoxOnHole -> True
    _         -> False

step :: GameState -> Action -> GameState
step gameState action = flip execState gameState $ runStep action

runStep :: Action -> State GameState ()
runStep action = do
  case action of
    Up      -> moveWorker $ direction action
    Down    -> moveWorker $ direction action
    Left    -> moveWorker $ direction action
    Right   -> moveWorker $ direction action
    Restart -> restartLevel
    _       -> return ()
    -- now compare the sets and check the game completion
  holes <- use holes
  boxes <- use boxes
  when (holes == boxes) $ error "Level complete!"
  where
    restartLevel :: State GameState ()
    restartLevel = do
      originCells <- head <$> use undoStack
      m <- use height
      n <- use width
      boxes .= S.empty
      forM_ [0 .. m - 1] $ \i -> do
        forM_ [0 .. n - 1] $ \j -> do
          let p = Point i j
          c <- getValidCell p
          when (isWorker c) $ worker .= Point i j
          when (isBox c) $ do
            bxs <- use boxes
            boxes .= (S.insert p bxs)
      cells .= originCells
      return ()
    moveWorker :: Direction -> State GameState ()
    moveWorker d = do
      cs0 <- use cells
      point0 <- use worker
      let point1 = moveDir point0 d
      let point2 = moveDir point1 d
      c0 <- directWorker d <$> getValidCell point0
      c1 <- getValidCell point1
      c2 <- getValidCell point2
      let ((d0, d1, d2), moveStatus) = move (c0, c1, c2)
        --      name .= T.pack (" c012 = " <> show (c0, c1, c2) <> " d012 = " <> show (d0, d1, d2) <> "             ")
      case moveStatus of
        Just True
          -- moved both worker and box
         -> do
          worker .= point1
          let cs1 = updateCell cs0 point0 d0
          let cs2 = updateCell cs1 point1 d1
          let cs3 = updateCell cs2 point2 d2
          cells .= cs3
        Just False
          -- moved worker only
         -> do
          worker .= point1
          let cs1 = updateCell cs0 point0 d0
          let cs2 = updateCell cs1 point1 d1
          cells .= cs2
        Nothing
          -- nothing moved (but the worker could change the direction)
         -> do
          let cs1 = updateCell cs0 point0 d0
          cells .= cs1
      return ()
    direction :: Action -> Direction
    direction a =
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
    getValidCell :: Point -> State GameState Cell
    getValidCell p = do
      cs <- use cells
      m <- use height
      n <- use width
      let i = p ^. _Point . _1
      let j = p ^. _Point . _2
      if 0 <= i && i < m && 0 <= j && j < n
        then return $ getCell cs p
        else return Wall

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

getCell :: MatrixCell -> Point -> Cell
getCell mtx p =
  let Point i j = p
   in (mtx ! i) ! j

updateCell :: MatrixCell -> Point -> Cell -> MatrixCell
updateCell mtx p cell
  -- awesome lenses to access 2d vector
  --   in let row = V.modify (\v -> W.write v j cell) (mtx ! i)
  --       in V.modify (\vv -> W.write vv i row) mtx
 =
  let Point i j = p
   in mtx & ix i . ix j .~ cell

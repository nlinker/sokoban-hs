{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE TupleSections         #-}

module Sokoban.Model where

import Prelude hiding (Left, Right)

import Control.Lens        (Lens', lens, use, (&), (+~), (.=), (.~), (^.))
import Control.Lens.TH     (makeLenses)
import Control.Monad.State (State, execState)
import Data.Hashable       (Hashable)
import Data.Vector         (Vector, (!))
import Debug.Trace         (traceM)
import GHC.Generics        (Generic)
import Sokoban.Level       (Cell(..), Direction(..), Level)
import Control.Monad (when)

import qualified Data.HashSet        as S
import qualified Data.Text           as T
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as W
import qualified Sokoban.Level       as L (cells, height, name, width)

data Point =
  Point Int Int
  deriving (Eq, Show, Generic, Hashable)

--makePrisms ''Point
p_1 :: Lens' Point Int
p_1 =
  lens
    (\p ->
       let Point i _ = p
        in i)
    (\p v ->
       let Point _ j = p
        in Point v j)

p_2 :: Lens' Point Int
p_2 =
  lens
    (\p ->
       let Point _ j = p
        in j)
    (\p v ->
       let Point i _ = p
        in Point i v)

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
    else return $
         GameState
           { _cells = V.fromList $ V.fromList <$> levelCells
           , _height = m
           , _width = n
           , _name = level ^. L.name
           , _worker = fst $ head workers
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
runStep action =
  case action of
    Up    -> moveWorker $ direction action
    Down  -> moveWorker $ direction action
    Left  -> moveWorker $ direction action
    Right -> moveWorker $ direction action
    _     -> return ()
  where
    direction a =
      case a of
        Up    -> U
        Down  -> D
        Left  -> L
        Right -> R
        _     -> error $ "Should not happen: " <> show a
    moveWorker :: Direction -> State GameState ()
    moveWorker d = do
      m <- use height
      n <- use width
      cells0 <- use cells
      worker0 <- use worker
      let worker1 = move worker0 d
      let box1 = if isBox $ getCell cells0 worker1 then Just (move worker1 d)
            else Nothing
       
      let i = worker1 ^. p_1
      let j = worker1 ^. p_2
      when (0 <= i && i < m && 0 <= j && j < n) $
        when ((isBox $ getCell cells0 worker1) && (isEmptyOrHole $ getCell cells0 box1)) $ do
          -- the move is valid
          return ()
            
      do
          let c0 = getCell cells0 worker0
          let c1 = getCell cells0 worker1
          cFromTo' <- checkValidMove d c0 c1
          case cFromTo' of
            Nothing -> do
              return ()
            Just (cFrom, cTo) -> do
              return ()
          case c1 of
            Worker _ -> traceM $ "Impossible destination cell: " <> show c1
            WorkerOnHole _ -> traceM $ "Impossible destination cell: " <> show c1
            Hole -> do
              clearFromPosition c0 d c1
              -- new cell will become worker
              worker .= worker1
              cells1 <- use cells
              cells .= updateCell cells1 worker1 (WorkerOnHole d)
            Wall -> do
              clearFromPosition c0 d c1
              cells1 <- use cells
              cells .= cells1
            Empty -> do
              let cells1 = updateCell cells0 worker0 c0
              -- new cell will become worker
              let cells2 = updateCell cells1 worker1 (Worker d)
              worker .= worker1
              cells .= cells2
            Box -> do
              let cells1 = updateCell cells0 worker0 c0
              -- move box if possible
              worker .= worker1
              cells .= cells1
            BoxOnHole -> do
              let cells1 = updateCell cells0 worker0 c0
              -- move box if possible
              worker .= worker1
              cells .= cells1
      return ()
    checkValidMove :: Direction -> Cell -> Cell -> State GameState (Maybe (Cell, Cell))
    checkValidMove d src dst = do
      let cellFrom =
            case src of
              WorkerOnHole _ -> Hole
              _              -> Empty
      let cellTo' =
            case dst of
              Hole      -> Just (WorkerOnHole d)
              Box       -> Just (Worker d)
              BoxOnHole -> Just (WorkerOnHole d)
              Empty     -> Just (Worker d)
              _         -> Nothing
      return $ (cellFrom, ) <$> cellTo'
    clearFromPosition :: Cell -> Direction -> Cell -> State GameState ()
    clearFromPosition src d dst = do
      worker0 <- use worker
      cells0 <- use cells
      let cellFrom =
            case src of
              WorkerOnHole _ -> Hole
              _              -> Empty
      let cellTo' =
            case dst of
              Empty     -> Just (Worker d)
              Hole      -> Just (WorkerOnHole d)
              Box       -> Just (Worker d)
              BoxOnHole -> Just (WorkerOnHole d)
              _         -> Nothing
      case cellTo' of
        Nothing -> return ()
        Just cellTo ->
          case dst of
            Worker _       -> return ()
            WorkerOnHole _ -> return ()
            Hole           -> cells .= updateCell cells0 worker0 cellFrom
            Box            -> cells .= updateCell cells0 worker0 cellFrom
            BoxOnHole      -> cells .= updateCell cells0 worker0 cellFrom
            Wall           -> return ()
            Empty          -> cells .= updateCell cells0 worker0 cellFrom

-- We use screen (not Decartes) coordinates (i, j).
-- The origin is in the upper left corner.
move :: Point -> Direction -> Point
move p d =
  case d of
    U -> p & p_1 +~ -1
    D -> p & p_1 +~ 1
    L -> p & p_2 +~ -1
    R -> p & p_2 +~ 1

getCell :: MatrixCell -> Point -> Cell
getCell mtx p =
  let Point i j = p
   in (mtx ! i) ! j

updateCell :: MatrixCell -> Point -> Cell -> MatrixCell
updateCell mtx p cell =
  let Point i j = p
   in let row = V.modify (\v -> W.write v j cell) (mtx ! i)
       in V.modify (\vv -> W.write vv i row) mtx

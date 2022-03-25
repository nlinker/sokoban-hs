{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Sokoban.Model where

import Prelude hiding (Left, Right)

import Control.Lens        (ix, use, (%=), (&), (+~), (.=), (.~), (^.), _1, _2, _3)
import Control.Lens.TH     (makeLenses, makePrisms)
import Control.Monad       (forM_, unless, when)
import Control.Monad.State (MonadState, evalState, execState)
import Data.Hashable       (Hashable)
import Data.Vector         (Vector, (!))
import GHC.Generics        (Generic)
import Sokoban.Level       (Cell(..), Direction(..), Level, LevelCollection, levels)

import qualified Data.HashSet           as S
import qualified Data.Text              as T
import qualified Data.Vector            as V
import qualified Sokoban.Level          as L (cells, height, id, width)

data Point =
  Point Int Int
  deriving (Eq, Show, Generic, Hashable)

makePrisms ''Point

type MatrixCell = Vector (Vector Cell)

data Diff =
  Diff
    { _point     :: !Point
    , _direction :: !Direction
    , _cell0     :: !Cell
    , _cell1     :: !Cell
    , _cell2     :: !Cell
    }
  deriving (Eq, Show)

makeLenses ''Diff

data LevelState =
  LevelState
    { _id         :: !T.Text
    , _cells      :: MatrixCell
    , _origin     :: MatrixCell
    , _height     :: !Int
    , _width      :: !Int
    , _worker     :: !Point
    , _boxes      :: S.HashSet Point
    , _goals      :: S.HashSet Point
    , _isComplete :: !Bool
    , _undoStack  :: ![Diff]
    , _undoIndex  :: !Int
    , _clicks     :: ![Point]
    , _message    :: !T.Text
    }
  deriving (Eq, Show)

makeLenses ''LevelState

data GameState =
  GameState
    { _collection :: !LevelCollection
    , _index      :: !Int
    , _levelState :: !LevelState
    }
  deriving (Eq, Show)

makeLenses ''GameState

data Action
  = Up
  | Down
  | Left
  | Right
  | Undo
  | Redo
  | Restart
  | PrevLevel
  | NextLevel
  | MoveBoxes [Point] [Point]
  | MoveWorker Point
  | Debug
  deriving (Eq, Show)

interpretClick :: GameState -> (Point, Bool) -> Maybe Action
interpretClick gameState click = evalState evalClick gameState
  where
    evalClick :: MonadState GameState m => m (Maybe Action)
    evalClick = do
      let (p, _) = click
      _clks <-
        (\cs ->
           if p `elem` cs
             then filter (== p) cs
             else p : cs) <$>
        use (levelState . clicks)
      cell <- getCell p
      return $
        case cell of
          Worker _       -> Nothing
          WorkerOnGoal _ -> Nothing
          Goal           -> Nothing
          Box            -> Nothing
          BoxOnGoal      -> Nothing
          Empty          -> Nothing
          Wall           -> Nothing

step :: GameState -> Action -> GameState
step gameState action = (execState $ runStep action) gameState

runStep :: MonadState GameState m => Action -> m ()
runStep action = do
  case action of
    Up        -> moveWorker (toDirection action) False
    Down      -> moveWorker (toDirection action) False
    Left      -> moveWorker (toDirection action) False
    Right     -> moveWorker (toDirection action) False
    Restart   -> restartLevel
    Undo      -> undoMove
    Redo      -> redoMove
    PrevLevel -> switchLevel (negate 1)
    NextLevel -> switchLevel (0 + 1)
    Debug     -> dumpState
    _         -> return ()
    -- now compare the sets and check the game completion
  ls <- use levelState
  if ls ^. goals == ls ^. boxes
    then do
      levelState . isComplete .= True
      levelState . message .= "Level complete!"
    else levelState . isComplete .= False
  -- dumpState

dumpState :: MonadState GameState m => m ()
dumpState = do
  undos <- use $ levelState . undoStack
  uidx <- use $ levelState . undoIndex
  let msg1 = "uidx: " <> show uidx <> "\n"
  let msg2 = msg1 <> concatMap (\x -> show x <> "\n") undos
  levelState . message .= T.pack msg2
  return ()

restartLevel :: MonadState GameState m => m ()
restartLevel = do
  originCells <- use $ levelState . origin
  case extractWBH originCells of
    Nothing -> error $ "Invariant violation: " <> show originCells
    Just (w, b, h) -> do
      levelState . worker .= w
      levelState . boxes .= b
      levelState . goals .= h
      levelState . cells .= originCells
      levelState . undoStack .= []
      levelState . undoIndex .= -1
  return ()

switchLevel :: MonadState GameState m => Int -> m ()
switchLevel di = do
  idx <- (+ di) <$> use index
  levels <- use (collection . levels)
  levelState . message .= T.pack ("switchLevel: length levels = " <> show (length levels))
  let newIdx
        | idx < 0 = 0
        | idx > length levels - 1 = length levels - 1
        | otherwise = idx
  -- it does update the state for sure, since it has already been verified during the parsing
  forM_ (initial (levels !! newIdx)) $ \l -> do
    index .= newIdx
    levelState .= l

redoMove :: MonadState GameState m => m ()
redoMove = do
  undos <- use $ levelState . undoStack
  uidx <- use $ levelState . undoIndex
  when (0 < uidx && uidx <= length undos) $ do
    let diff = undos !! (uidx - 1)
    moveWorker (diff ^. direction) True
    levelState . undoIndex .= uidx - 1

undoMove :: MonadState GameState m => m ()
undoMove = do
  undos <- use $ levelState . undoStack
  uidx <- use $ levelState . undoIndex
  when (0 <= uidx && uidx < length undos) $ do
    let diff = undos !! uidx
    let point0 = diff ^. point
    let point1 = moveDir point0 $ diff ^. direction
    let point2 = moveDir point1 $ diff ^. direction
    updateCell point0 $ diff ^. cell0
    updateCell point1 $ diff ^. cell1
    updateCell point2 $ diff ^. cell2
    levelState . undoIndex .= uidx + 1
    -- rebuild levelState
    cells <- use (levelState . cells)
    case extractWBH cells of
      Nothing -> error $ "Invariant violation: " <> show cells
      Just (w, b, h) -> do
        levelState . worker .= w
        levelState . boxes .= b
        levelState . goals .= h

moveWorker :: MonadState GameState m => Direction -> Bool -> m ()
moveWorker d redoing = do
  ls <- use levelState
  let allowToMove = not $ ls ^. isComplete
  when allowToMove $ do
    let point0 = ls ^. worker
    let point1 = moveDir point0 d
    let point2 = moveDir point1 d
    c0' <- getCell point0
    c1 <- getCell point1
    c2 <- getCell point2
    let c0 = directWorker d c0'
    -- cells c0 can differ from c0' in direction only, and must be Worker / WorkerOnGoal cell
    let ((d0, d1, d2), moveStatus) = move (c0, c1, c2)
    case moveStatus of
      Just True
        -- moved both worker and box
       -> do
        levelState . boxes %= (S.insert point2 . S.delete point1)
        levelState . worker .= point1
        updateCell point0 d0
        updateCell point1 d1
        updateCell point2 d2
      Just False
        -- moved worker only
       -> do
        levelState . worker .= point1
        updateCell point0 d0
        updateCell point1 d1
      Nothing
        -- nothing moved (but the worker could change the direction)
       -> updateCell point0 d0
    unless redoing $
      -- now update the undo stack
      -- this variant makes redirections also to be recorded and undoable
      -- > let diff = Diff {_point = point0, _direction = d, _cell0 = c0, _cell1 = c1, _cell2 = c2}
      -- > when ((d0, d1, d2) /= (c0, c1, c2)) $ undoStack %= (diff :)
     do
      let diff = Diff {_point = point0, _direction = d, _cell0 = c0, _cell1 = c1, _cell2 = c2}
      when ((d0, d1, d2) /= (c0, c1, c2)) $ do
        let uidx = ls ^. undoIndex
        -- undoStack: [diff3, diff2, diff1, diff0]  =>  [diff, d1, d0]
        -- undoIndex:                 ^2^           =>    ^0^
        levelState . undoStack .= diff : drop uidx (ls ^. undoStack)
        levelState . undoIndex .= 0

toDirection :: Action -> Direction
toDirection a =
  case a of
    Up    -> U
    Down  -> D
    Left  -> L
    Right -> R
    _     -> error $ "Should not happen: " <> show a

directWorker :: Direction -> Cell -> Cell
directWorker d cw =
  case cw of
    Worker _       -> Worker d
    WorkerOnGoal _ -> WorkerOnGoal d
    cell           -> cell

---------------------------------------------------------------------------------------------
-- build the initial state
initial :: Level -> Maybe LevelState
initial level = do
  let m = level ^. L.height
  let n = level ^. L.width
  let levelCells = level ^. L.cells
  let cells = V.fromList $ V.fromList <$> levelCells
  case extractWBH cells of
    Nothing -> Nothing
    Just (worker, boxes, goals) ->
      return $
      LevelState
        { _id = level ^. L.id
        , _cells = cells
        , _origin = cells
        , _height = m
        , _width = n
        , _worker = worker
        , _boxes = boxes
        , _goals = goals
        , _isComplete = False
        , _undoStack = []
        , _undoIndex = -1
        , _clicks = []
        , _message = "Controls: ← ↑ → ↓ R U I PgUp PgDn"
        }

-- extract worker, boxes and goals, needed to be run after start, restart or undo
extractWBH :: MatrixCell -> Maybe (Point, S.HashSet Point, S.HashSet Point)
extractWBH xs =
  let (workers, boxes, goals) = execState extract ([], [], [])
      workersCount = length workers
      boxesCount = length boxes
      goalsCount = length goals
   in if workersCount == 1 && boxesCount > 0 && boxesCount == goalsCount
        then Just (head workers, S.fromList boxes, S.fromList goals)
        else Nothing
  where
    extract = do
      let m = V.length xs
      let n = V.length (xs ! 0)
      forM_ [0 .. m - 1] $ \i ->
        forM_ [0 .. n - 1] $ \j -> do
          let x = (xs ! i) ! j
          when (isWorker x) $ _1 %= (Point i j :)
          when (isBox x) $ _2 %= (Point i j :)
          when (isGoal x) $ _3 %= (Point i j :)

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

move :: (Cell, Cell, Cell) -> ((Cell, Cell, Cell), Maybe Bool)
move triple =
  case triple of
    (Worker d, Box, Empty)             -> ((Empty, Worker d, Box), Just True)
    (Worker d, Box, Goal)              -> ((Empty, Worker d, BoxOnGoal), Just True)
    (Worker d, BoxOnGoal, Empty)       -> ((Empty, WorkerOnGoal d, Box), Just True)
    (Worker d, BoxOnGoal, Goal)        -> ((Empty, WorkerOnGoal d, BoxOnGoal), Just True)
    (WorkerOnGoal d, Box, Empty)       -> ((Goal, Worker d, Box), Just True)
    (WorkerOnGoal d, Box, Goal)        -> ((Goal, Worker d, BoxOnGoal), Just True)
    (WorkerOnGoal d, BoxOnGoal, Empty) -> ((Goal, WorkerOnGoal d, Box), Just True)
    (WorkerOnGoal d, BoxOnGoal, Goal)  -> ((Goal, WorkerOnGoal d, BoxOnGoal), Just True)
    (Worker d, Empty, c3)              -> ((Empty, Worker d, c3), Just False)
    (Worker d, Goal, c3)               -> ((Empty, WorkerOnGoal d, c3), Just False)
    (WorkerOnGoal d, Empty, c3)        -> ((Goal, Worker d, c3), Just False)
    (WorkerOnGoal d, Goal, c3)         -> ((Goal, WorkerOnGoal d, c3), Just False)
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

getCell :: MonadState GameState m => Point -> m Cell
getCell p = do
  ls <- use levelState
  let m = ls ^. height
  let n = ls ^. width
  let Point i j = p
  if 0 <= i && i < m && 0 <= j && j < n
    then return $ ((ls ^. cells) ! i) ! j
    else return Wall

updateCell :: MonadState GameState m => Point -> Cell -> m ()
updateCell p cell = do
  ls <- use levelState
  let cs = ls ^. cells
  let m = ls ^. height
  let n = ls ^. width
  let Point i j = p
  -- awesome lenses to access 2d vector
  --   in let row = V.modify (\v -> W.write v j cell) (mtx ! i)
  --       in V.modify (\vv -> W.write vv i row) mtx
  when (0 <= i && i < m && 0 <= j && j < n) $ levelState . cells .= (cs & ix i . ix j .~ cell)

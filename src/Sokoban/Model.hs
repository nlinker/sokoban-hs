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
import Control.Monad       (forM_, when, unless)
import Control.Monad.State (MonadState, evalState, execState)
import Data.Hashable       (Hashable)
import Data.Vector         (Vector, (!))
import GHC.Generics        (Generic)
import Sokoban.Level       (Cell(..), Direction(..), Level, LevelCollection, levels)

import qualified Data.HashSet  as S
import qualified Data.Text     as T
import qualified Data.Vector   as V
import qualified Sokoban.Level as L (cells, height, id, width)

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
    , _holes      :: S.HashSet Point
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
  | MoveBoxStart Point
  | MoveBoxEnd Point
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
          WorkerOnHole _ -> Nothing
          Hole           -> Nothing
          Box            -> Nothing
          BoxOnHole      -> Nothing
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
  if ls ^. holes == ls ^. boxes
    then do
      levelState . isComplete .= True
      levelState . message .= "Level complete!"
    else levelState . isComplete .= False
  -- dumpState
  where
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
          levelState . holes .= h
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
            levelState . holes .= h
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
        -- cells c0 can differ from c0' in direction only, and must be Worker / WorkerOnHole cell
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
        
        unless redoing $ do
          -- now update the undo stack
          -- this variant makes redirections also to be recorded and undoable
          -- > let diff = Diff {_point = point0, _direction = d, _cell0 = c0, _cell1 = c1, _cell2 = c2}
          -- > when ((d0, d1, d2) /= (c0, c1, c2)) $ undoStack %= (diff :)
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
        WorkerOnHole _ -> WorkerOnHole d
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
    Just (worker, boxes, holes) ->
      return $
      LevelState
        { _id = level ^. L.id
        , _cells = cells
        , _origin = cells
        , _height = m
        , _width = n
        , _worker = worker
        , _boxes = boxes
        , _holes = holes
        , _isComplete = False
        , _undoStack = []
        , _undoIndex = -1
        , _clicks = []
        , _message = "Controls: ← ↑ → ↓ R U I PgUp PgDn"
        }

-- extract worker, boxes and holes, needed to be run after start, restart or undo
extractWBH :: MatrixCell -> Maybe (Point, S.HashSet Point, S.HashSet Point)
extractWBH xs =
  let (workers, boxes, holes) = execState extract ([], [], [])
      workersCount = length workers
      boxesCount = length boxes
      holesCount = length holes
   in if workersCount == 1 && boxesCount > 0 && boxesCount == holesCount
        then Just (head workers, S.fromList boxes, S.fromList holes)
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
          when (isHole x) $ _3 %= (Point i j :)

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

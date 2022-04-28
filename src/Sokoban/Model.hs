{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Sokoban.Model where

import           Prelude hiding (Left, Right, id)
import qualified Prelude as P

import Control.Arrow              (second)
import Control.Lens               (Lens', ix, lens, use, (%=), (&), (+=), (-=), (.=), (.~), (<>=),
                                   (^.), _1, _2, _3)
import Control.Lens.TH            (makeLenses, makePrisms)
import Control.Monad              (filterM, forM, forM_, unless, when)
import Control.Monad.State.Strict (MonadState, evalState, execState, get, gets, runState)
import Data.Foldable              (foldl', minimumBy)
import Data.Ord                   (comparing)
import Data.Vector                (Vector, (!))
import Sokoban.Debug              (getDebugModeM, setDebugModeM)
import Sokoban.Level              (Cell(..), Direction(..), Level, LevelCollection, PD(..),
                                   Point(..), deriveDir, isBox, isEmptyOrGoal, isGoal, isWorker,
                                   levels, movePoint, opposite, w8FromDirection, w8ToDirection, _PD)
import Sokoban.Solver             (AStarSolver(..), aStarFind, breadFirstFind)
import Text.InterpolatedString.QM (qm)

import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S
import qualified Data.Text           as T
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as VU
import qualified Sokoban.Level       as L (cells, height, id, width)
import qualified Text.Builder        as TB

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Debug.Trace             (trace, traceM)
import System.IO.Unsafe        (unsafePerformIO)

type MatrixCell = Vector (Vector Cell)

data Diff =
  Diff
    { _direction :: !Direction
    , _isPush    :: !Bool
    }
  deriving (Eq, Show)

newtype UndoItem =
  UndoItem [Diff]
  deriving (Eq, Show)

data LevelState =
  LevelState
    { _id         :: !T.Text
    , _cells      :: !MatrixCell
    , _origin     :: !MatrixCell
    , _height     :: !Int
    , _width      :: !Int
    , _worker     :: !Point
    , _boxes      :: !(S.HashSet Point)
    , _goals      :: !(S.HashSet Point)
    , _isComplete :: !Bool
    , _undoStack  :: ![UndoItem]
    , _undoIndex  :: !Int
    , _stats      :: !Stats
    }
  deriving (Eq, Show)

data AnimationMode
  = AnimationDo
  | AnimationUndo
  | AnimationRedo
  deriving (Eq, Show)

data ViewState =
  ViewState
    { _doClearScreen   :: !Bool
    , _clicks          :: ![Point]
    , _destinations    :: !(S.HashSet Point)
    , _animateRequired :: !Bool
    , _animationMode   :: !AnimationMode
    , _message         :: !T.Text
    }
  deriving (Eq, Show)

data Stats =
  Stats
    { _moveCount :: !Int
    , _pushCount :: !Int
    }
  deriving (Eq, Show)

data GameState =
  GameState
    { _collection :: !LevelCollection
    , _index      :: !Int
    , _levelState :: !LevelState
    , _viewState  :: !ViewState
    }
  deriving (Eq, Show)

data FlatLevelState =
  FlatLevelState
    { _flatCells  :: !(VU.Vector Cell)
    , _flatHeight :: !Int
    , _flatWidth  :: !Int
    , _flatWorker :: !Int
    , _flatBoxes  :: !(VU.Vector Int)
    , _flatGoals  :: !(VU.Vector Int)
    }
  deriving (Eq, Show)

makeLenses ''Diff

makeLenses ''LevelState

makeLenses ''ViewState

makeLenses ''Stats

makeLenses ''FlatLevelState

makeLenses ''GameState

makePrisms ''UndoItem

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
  | SelectBox Point
  | SelectWorker
  | MoveBoxes [Point] [Point]
  | MoveWorker Point
  | ToggleDebugMode
  deriving (Eq, Show)

step :: GameState -> Action -> GameState
step gameState action = (execState $ runStep action) gameState

runStep :: MonadState GameState m => Action -> m ()
runStep action = do
  case action of
    Up                -> moveWorker (toDirection action) True
    Down              -> moveWorker (toDirection action) True
    Left              -> moveWorker (toDirection action) True
    Right             -> moveWorker (toDirection action) True
    Restart           -> restartLevel
    Undo              -> undoMoveWorker
    Redo              -> redoMoveWorker
    PrevLevel         -> switchLevel (negate 1)
    NextLevel         -> switchLevel (0 + 1)
    MoveWorker dst    -> moveWorkerAlongPath dst
    MoveBoxes src dst -> moveBoxesByWorker src dst
    SelectWorker      -> computeWorkerReachability
    SelectBox box     -> computeBoxReachability box
    ToggleDebugMode   -> toggleDebugMode
  resetView action

resetView :: MonadState GameState m => Action -> m ()
resetView action = do
  case action of
    NextLevel -> viewState . doClearScreen .= True
    PrevLevel -> viewState . doClearScreen .= True
    ToggleDebugMode -> viewState . doClearScreen .= True
    SelectWorker -> return ()
    SelectBox _ -> return ()
    Restart -> do
      viewState . message .= ""
      viewState . clicks .= []
      viewState . destinations .= S.empty
      viewState . doClearScreen .= True
    _ -> do
      viewState . clicks .= []
      viewState . destinations .= S.empty
      complete <- use (levelState . isComplete)
      unless complete $ viewState . message %= \msg -> T.replicate (T.length msg) " "
  -- now compare the sets and check the game completion
  ls <- use levelState
  if ls ^. goals == ls ^. boxes
    then do
      levelState . isComplete .= True
      viewState . doClearScreen .= True
      viewState . message .= T.pack "Level complete!"
    else levelState . isComplete .= False

toggleDebugMode :: MonadState GameState m => m ()
toggleDebugMode = do
  dm <- getDebugModeM
  setDebugModeM $ not dm
  viewState . message .= [qm| Set debug mode: {dm} -> {not dm}|]

restartLevel :: MonadState GameState m => m ()
restartLevel = do
  originCells <- use (levelState . origin)
  case extractWBH originCells of
    Nothing -> error $ "Invariant violation: " <> show originCells
    Just (w, b, h) -> do
      levelState . worker .= w
      levelState . boxes .= b
      levelState . goals .= h
      levelState . cells .= originCells
      levelState . undoStack .= []
      levelState . undoIndex .= -1

switchLevel :: MonadState GameState m => Int -> m ()
switchLevel di = do
  idx <- (+ di) <$> use index
  levels <- use (collection . levels)
  let newIdx
        | idx < 0 = 0
        | idx > length levels - 1 = length levels - 1
        | otherwise = idx
  -- it does update the state for sure, since it has already been verified during the parsing
  forM_ (initialLevelState (levels !! newIdx)) $ \level -> do
    index .= newIdx
    levelState .= level

doMove :: MonadState GameState m => Direction -> m (Maybe Diff)
doMove d = do
  ls <- use levelState
  if not $ ls ^. isComplete -- if allow to move
    then do
      let point0 = ls ^. worker
      let point1 = movePoint point0 d
      let point2 = movePoint point1 d
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
          levelState . stats . pushCount += 1
          levelState . boxes %= (S.insert point2 . S.delete point1)
          levelState . worker .= point1
          updateCell point0 d0
          updateCell point1 d1
          updateCell point2 d2
          return $ Just $ Diff {_direction = d, _isPush = True}
        Just False
        -- moved worker only
         -> do
          levelState . stats . moveCount += 1
          levelState . worker .= point1
          updateCell point0 d0
          updateCell point1 d1
          return $ Just $ Diff {_direction = d, _isPush = False}
        Nothing
        -- nothing moved (but the worker could change the direction)
         -> do
          updateCell point0 d0
          return Nothing
    else return Nothing

undoMove :: MonadState GameState m => Diff -> m ()
undoMove diff = do
  let dir = diff ^. direction
  point1 <- use (levelState . worker)
  let point0 = movePoint point1 $ opposite dir
  let point2 = movePoint point1 dir
  d0 <- getCell point0
  d1 <- getCell point1
  d2 <- getCell point2
  let (c0, c1, c2) = unMove (d0, d1, d2) $ diff ^. isPush
  updateCell point0 $ directWorker dir c0
  updateCell point1 c1
  updateCell point2 c2
  if diff ^. isPush
    then levelState . stats . pushCount -= 1
    else levelState . stats . moveCount -= 1
  levelState . worker .= point0

redoMoveWorker :: MonadState GameState m => m ()
redoMoveWorker = do
  undos <- use (levelState . undoStack)
  uidx <- use (levelState . undoIndex)
  when (0 < uidx && uidx <= length undos) $ do
    let UndoItem diffs = undos !! (uidx - 1)
    forM_ diffs $ doMove . (^. direction)
    when (length diffs > 1) $ do
      viewState . animateRequired .= True
      viewState . animationMode .= AnimationRedo
    levelState . undoIndex .= uidx - 1

undoMoveWorker :: MonadState GameState m => m ()
undoMoveWorker = do
  undos <- use (levelState . undoStack)
  uidx <- use (levelState . undoIndex)
  when (0 <= uidx && uidx < length undos) $ do
    let UndoItem diffs = undos !! uidx
    forM_ (reverse diffs) undoMove
    when (length diffs > 1) $ do
      viewState . animateRequired .= True
      viewState . animationMode .= AnimationUndo
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
moveWorker d storeUndo = do
  diff' <- doMove d
  case diff' of
    Nothing -> return ()
    Just diff ->
      when storeUndo $ do
        ls <- use levelState
        let uidx = ls ^. undoIndex
        -- undoStack: [diff3, diff2, diff1, diff0]  =>  [diff, d1, d0]
        -- undoIndex:                 ^2^           =>    ^0^
        levelState . undoStack .= UndoItem [diff] : drop uidx (ls ^. undoStack)
        levelState . undoIndex .= 0

moveWorkerAlongPath :: MonadState GameState m => Point -> m ()
moveWorkerAlongPath dst = do
  src <- use (levelState . worker)
  solver <- buildMoveSolver []
  dirs <- pathToDirections <$> aStarFind solver src dst (return . (== dst))
  diffs' <- sequenceA <$> mapM doMove dirs
  case diffs' of
    Nothing -> return ()
    Just [] -> return ()
    Just diffs -> do
      ls <- use levelState
      let uidx = ls ^. undoIndex
      levelState . undoStack .= UndoItem diffs : drop uidx (ls ^. undoStack)
      levelState . undoIndex .= 0
      viewState . animateRequired .= True
      viewState . animationMode .= AnimationDo

computeWorkerReachability :: MonadState GameState m => m ()
computeWorkerReachability = do
  gs <- get
  w <- use (levelState . worker)
  let area = evalState (findWorkerArea w) gs
  let withoutW = S.delete w area
  viewState . destinations .= withoutW
  where
    findWorkerArea :: MonadState GameState m => Point -> m (S.HashSet Point)
    findWorkerArea s = do
      moveSolver <- buildMoveSolver []
      area <- breadFirstFind moveSolver s
      return $ S.fromList area

computeBoxReachability :: MonadState GameState m => Point -> m ()
computeBoxReachability box = do
  boxez <- use (levelState . boxes)
  when (S.member box boxez) $ do
    egs <- gets $ eraseBoxes [box]
    let area = evalState (findBoxArea box) egs
    let withoutBox = S.delete box area
    viewState . destinations .= withoutBox
  where
    findBoxArea :: MonadState GameState m => Point -> m (S.HashSet Point)
    findBoxArea s = do
      sources <- findBoxDirections s
      areas <-
        forM sources $ \src -> do
          pushSolver <- buildPushSolver -- :: AStarSolver m PD
          breadFirstFind pushSolver src
      let commonArea = (^. (_PD . _1)) <$> concat (filter (not . null) areas)
      return $ S.fromList commonArea

moveBoxesByWorker :: MonadState GameState m => [Point] -> [Point] -> m ()
moveBoxesByWorker src dst = do
  dirs <-
    case (src, dst) of
      ([s], [t]) -> do
        erasedGs <- gets $ eraseBoxes [s]
        -- erase source box not to break path finding and avoid spoiling of the current gs
        let (dirs, _dbgGs) = runState (tryMove1Box s t) erasedGs
        -- viewState . message .= (dbgGs ^. viewState . message)
        -- viewState . doClearScreen .= True
        return dirs
      ([s1, s2], [t1, t2]) -> do
        erasedGs <- gets $ eraseBoxes [s1, s2]
        let (dirs, _dbgGs) = runState (tryMove2Boxes [s1, s2] [t1, t2]) erasedGs
        -- viewState . message .= (dbgGs ^. viewState . message)
        -- viewState . doClearScreen .= True
        return dirs
      _ -> return []
  diffs' <- sequenceA <$> mapM doMove dirs
  case diffs' of
    Nothing -> return ()
    Just [] -> return ()
    Just diffs -> do
      ls <- use levelState
      let uidx = ls ^. undoIndex
      levelState . undoStack .= UndoItem diffs : drop uidx (ls ^. undoStack)
      levelState . undoIndex .= 0
      viewState . animateRequired .= True
      viewState . animationMode .= AnimationDo
  where
    tryMove1Box :: MonadState GameState m => Point -> Point -> m [Direction]
    tryMove1Box s t = do
      srcs <- findBoxDirections s
      paths <-
        forM srcs $ \src -> do
          let dst = PD t D []
          let stopCond (PD p _ _) = return $ p == t
          pushSolver <- buildPushSolver -- :: AStarSolver m PD
          path <- aStarFind pushSolver src dst stopCond
          return $ pushPathToDirections path
      let nePaths = filter (not . null) paths
      let selected =
            if null nePaths
              then []
              else minimumBy (comparing length) nePaths
      return selected
    tryMove2Boxes :: MonadState GameState m => [Point] -> [Point] -> m [Direction]
    tryMove2Boxes _ss _ts = return []

-- erase source boxes to not break path finding and avoid spoil the current gs
eraseBoxes :: [Point] -> GameState -> GameState
eraseBoxes boxez gs =
  flip execState gs $ do
    guy <- use (levelState . worker)
    wc <- getCell guy
    case wc of
      Worker _       -> updateCell guy Empty
      WorkerOnGoal _ -> updateCell guy Goal
      _              -> return ()
    forM_ boxez $ \box -> do
      bc <- getCell box
      case bc of
        Box       -> updateCell box Empty
        BoxOnGoal -> updateCell box Goal
        _         -> return ()

findBoxDirections :: MonadState GameState m => Point -> m [PD]
findBoxDirections box = do
  moveSolver <- buildMoveSolver [box]
  let isAccessible p = isEmptyOrGoal <$> getCell p
  let tryBuildPath src dst = do
        accessible <- isAccessible dst
        if accessible
          then aStarFind moveSolver src dst (return . (== dst))
          else return []
  w <- use (levelState . worker)
  paths <- mapM (\d -> tryBuildPath w (movePoint box $ opposite d)) [U, D, L, R]
  let augPaths = zip [U, D, L, R] paths
  let dirPoints = uncurry (PD box) . second pathToDirections <$> filter (not . null . snd) augPaths
  return dirPoints

buildMoveSolver :: MonadState GameState m => [Point] -> m (AStarSolver m Point)
buildMoveSolver walls = do
  m <- use (levelState . height)
  n <- use (levelState . width)
  let p2int (Point i j) = i * n + j
  let int2p k = Point (k `div` n) (k `mod` n)
  let nodesBound = m * n
  return $
    AStarSolver
      { neighbors = neighbors
      , distance = distance
      , heuristic = heuristic
      , projection = p2int
      , injection = int2p
      , nodesBound = nodesBound
      , withCache = withCache
      }
  where
    neighbors p0 = do
      let isAccessible p =
            if p `elem` walls
              then return False
              else isEmptyOrGoal <$> getCell p
      let neighs = map (movePoint p0) [U, D, L, R]
      filterM isAccessible neighs
    distance np p0 = fromEnum (np /= p0)
    heuristic (Point i1 j1) (Point i2 j2) = return $ abs (i1 - i2) + abs (j1 - j2)

    pathCache :: MVar (H.HashMap (Point, Point) [Point])
    {-# NOINLINE pathCache #-}
    pathCache = unsafePerformIO $ newMVar H.empty

    withCache src dst algorithm = do
      let path' =
            unsafePerformIO $ do
              hm <- readMVar pathCache
              return $ H.lookup (src, dst) hm
      case path' of
        Just path -> return path
        Nothing -> do
          path <- algorithm
          return $ unsafePerformIO $ do
            dm <- getDebugModeM
            when dm $ traceM [qm| cacheUpdate {(src, dst)} -> {path} // {walls} |]
            modifyMVar pathCache (\hm -> return (H.insert (src, dst) path hm, path))

-- memoMoveSolver p0 = memo buildMoveSolver [p0]
buildPushSolver ::
     forall m. MonadState GameState m
  => m (AStarSolver m PD)
buildPushSolver = do
  m <- use (levelState . height)
  n <- use (levelState . width)
  let p2int (PD (Point i j) d _) = (i * n + j) * 4 + fromIntegral (w8FromDirection d)
  let int2p k =
        let kdir = k `mod` 4
            k4 = k `div` 4
         in PD (Point (k4 `div` n) (k4 `mod` n)) (w8ToDirection (fromIntegral kdir)) []
  -- let cache = M.empty :: M.HashMap Point (AStarSolver m Point)
  -- trace [qm| memoMoveSolver {p0}|] $
  let nodesBound = m * n * 4
  let neighbors (PD p0 d0 _ds0) = do
        moveSolver <- buildMoveSolver [p0]
        let isAccessible p = isEmptyOrGoal <$> getCell p
        let myFind src dst = aStarFind moveSolver src dst (return . (== dst))
        let tryBuildPath src dst = do
              accessible <- isAccessible dst
              if accessible
                then pathToDirections <$> myFind src dst
                else return []
          -- cont is the "continue push in the direction d0" neighbor
          -- src is the position of the worker for the push
          -- directed is the same box but with changed push direction
        cont <- filterM (\(PD p _ _) -> isAccessible p) [PD (movePoint p0 d0) d0 [d0]]
        let src = movePoint p0 (opposite d0)
        let otherDirs = filter (/= d0) [U, D, L, R]
        paths <- mapM (\d -> PD p0 d <$> tryBuildPath src (movePoint p0 $ opposite d)) otherDirs
        (cont <>) <$> filterM (\(PD _ _ ds) -> (return . not . null) ds) paths
        -- traceM [qm|  neighs = {neighs} |]
        -- neighs <- return neighs
  let heuristic (PD (Point i1 j1) d1 ds1) (PD (Point i2 j2) d2 _ds2) =
        return $ abs (i1 - i2) + abs (j1 - j2) + fromEnum (d1 /= d2) + length ds1
  let distance np p0 = fromEnum (np /= p0)
  return $
    AStarSolver
      { neighbors = neighbors
      , distance = distance
      , heuristic = heuristic
      , projection = p2int
      , injection = int2p
      , nodesBound = nodesBound
      , withCache = \_ _ alg -> alg
      }

--  let pathCache = unsafePerformIO $ newMVar H.empty
--      {-# NOINLINE pathCache #-}
--                then pathToDirections <$> myFind src dst
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
initialLevelState :: Level -> Maybe LevelState
initialLevelState level = do
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
        , _stats = Stats 0 0
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

-- mirroring of move function
unMove :: (Cell, Cell, Cell) -> Bool -> (Cell, Cell, Cell)
unMove triple isPush =
  case (triple, Just isPush) of
    ((Empty, Worker d, Box), Just True)             -> (Worker d, Box, Empty)
    ((Empty, Worker d, BoxOnGoal), Just True)       -> (Worker d, Box, Goal)
    ((Empty, WorkerOnGoal d, Box), Just True)       -> (Worker d, BoxOnGoal, Empty)
    ((Empty, WorkerOnGoal d, BoxOnGoal), Just True) -> (Worker d, BoxOnGoal, Goal)
    ((Goal, Worker d, Box), Just True)              -> (WorkerOnGoal d, Box, Empty)
    ((Goal, Worker d, BoxOnGoal), Just True)        -> (WorkerOnGoal d, Box, Goal)
    ((Goal, WorkerOnGoal d, Box), Just True)        -> (WorkerOnGoal d, BoxOnGoal, Empty)
    ((Goal, WorkerOnGoal d, BoxOnGoal), Just True)  -> (WorkerOnGoal d, BoxOnGoal, Goal)
    ((Empty, Worker d, c3), Just False)             -> (Worker d, Empty, c3)
    ((Empty, WorkerOnGoal d, c3), Just False)       -> (Worker d, Goal, c3)
    ((Goal, Worker d, c3), Just False)              -> (WorkerOnGoal d, Empty, c3)
    ((Goal, WorkerOnGoal d, c3), Just False)        -> (WorkerOnGoal d, Goal, c3)
    _                                               -> triple

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

showState :: GameState -> T.Text
showState gs =
  TB.run $ flip execState mempty $ do
    let ls = gs ^. levelState
    let cs = ls ^. cells
    let m = ls ^. height
    let n = ls ^. width
    let points = [Point i j | i <- [0 .. m - 1], j <- [0 .. n - 1]]
    forM_ points $ \p -> do
      let Point i j = p
      let ch = getCellBuilder $ (cs ! i) ! j
      this <>=
        if j /= 0
          then TB.char ' ' <> ch
          else ch
      when (j == n - 1) $ this <>= TB.char '\n'
    this <>= TB.text "; " <> TB.text (ls ^. id) <> TB.char '\n'
  where
    this :: Lens' a a
    this = lens P.id (\_ v -> v)
    getCellBuilder :: Cell -> TB.Builder
    getCellBuilder c =
      case c of
        Worker _       -> TB.char '@'
        WorkerOnGoal _ -> TB.char '+'
        Goal           -> TB.char '.'
        Box            -> TB.char '$'
        BoxOnGoal      -> TB.char '*'
        Empty          -> TB.char ' '
        Wall           -> TB.char '#'

pushPathToDirections :: [PD] -> [Direction]
pushPathToDirections pds = reverse $ foldl' (\acc pd -> reverse (pd ^. _PD . _3) <> acc) [] pds

pathToDirections :: [Point] -> [Direction]
pathToDirections ps = reverse $ convert ps []
  where
    convert [] _acc = []
    convert [_] acc = acc
    convert (p1:p2:ps) acc =
      case deriveDir p1 p2 of
        Nothing -> acc
        Just d  -> convert (p2 : ps) (d : acc)

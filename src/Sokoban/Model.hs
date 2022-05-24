{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
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
import Control.Monad.Primitive    (PrimMonad(..), PrimState)
import Control.Monad.ST.Strict    (runST)
import Control.Monad.State.Strict (MonadState, StateT, evalState, evalStateT, execState, get, gets,
                                   lift, runState)
import Data.Foldable              (foldl', minimumBy)
import Data.Maybe                 (catMaybes)
import Data.Ord                   (comparing)
import Data.Vector                (Vector, (!))
import Sokoban.Debug              (getDebugModeM, setDebugModeM)
import Sokoban.Level              (Cell(..), Direction(..), Level, LevelCollection, PD(..), PPD(..),
                                   Point(..), deriveDir, isBox, isEmptyOrGoal, isGoal, isWorker,
                                   levels, movePoint, opposite, ppdDir, ppdDirs, ppdFst, ppdIdx,
                                   ppdSelector, ppdSnd, w8FromDirection, w8ToDirection, _PD)
import Sokoban.Solver             (AStarSolver(..), aStarFind, breadFirstFind)
import Text.InterpolatedString.QM (qm)

import           Control.Monad.Identity     (runIdentity)
import           Control.Monad.Trans.Maybe  (MaybeT(..), runMaybeT)
import qualified Data.HashMap.Mutable.Basic as HM
import qualified Data.HashSet               as S
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import qualified Data.Vector.Unboxed        as VU
import           Debug.Trace                (traceM)
import qualified Sokoban.Level              as L (cells, height, id, width)
import qualified Text.Builder               as TB

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

data SolverContext m =
  SolverContext
    { _pathCache :: HM.MHashMap (PrimState m) ([Point], Direction, Point, Point) [Point]
    , _cHeight   :: Int
    , _cWidth    :: Int
    }
  deriving (Show)

makeLenses ''Diff

makeLenses ''LevelState

makeLenses ''ViewState

makeLenses ''Stats

makeLenses ''FlatLevelState

makeLenses ''GameState

makePrisms ''UndoItem

makeLenses ''SolverContext

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
resetView action
  -- now compare the sets and check the game completion
 = do
  ls <- use levelState
  if ls ^. goals == ls ^. boxes
    then do
      levelState . isComplete .= True
      viewState . doClearScreen .= True
      viewState . message .= T.pack "Level complete!"
    else levelState . isComplete .= False
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
      levelState . stats .= Stats 0 0

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
  gs <- get
  let m = gs ^. levelState . height
  let n = gs ^. levelState . width
  let dirs =
        runST $ do
          pc <- HM.new
          let ctx = SolverContext pc m n
          solver <- buildMoveSolver ctx dst []
          flip evalStateT gs $ aStarFind solver src
  diffs' <- sequenceA <$> mapM doMove (pathToDirections dirs)
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
  w <- use (levelState . worker)
  area <- S.delete w <$> findWorkerArea w
  viewState . destinations .= area
  where
    findWorkerArea ::
         forall m. MonadState GameState m
      => Point
      -> m (S.HashSet Point)
    findWorkerArea s = do
      gs <- get
      let m = gs ^. levelState . height
      let n = gs ^. levelState . width
      let someDst = gs ^. levelState . worker
      return $ runST $ do
        pc <- HM.new
        let ctx = SolverContext pc m n
        moveSolver <- buildMoveSolver ctx someDst []
        area <- flip evalStateT gs $ breadFirstFind moveSolver s
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
    findBoxArea s = do
      gs <- get
      let m = gs ^. levelState . height
      let n = gs ^. levelState . width
      let someDst = PD (gs ^. levelState . worker) U []
      let sources = findBoxDirections gs s
      let areas =
            runST $ do
              pc <- HM.new
              let ctx = SolverContext pc m n
              forM sources $ \src -> do
                pushSolver <- buildPushSolver ctx someDst
                flip evalStateT gs $ breadFirstFind pushSolver src
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
      gs <- get
      let srcs = findBoxDirections gs s
      let m = gs ^. levelState . height
      let n = gs ^. levelState . width
      paths <-
        forM srcs $ \src ->
          return $ runST $ do
            let dst = PD t D []
            hm <- HM.new
            let ctx = SolverContext hm m n
            pushSolver <- buildPushSolver ctx dst
            path <- flip evalStateT gs $ aStarFind pushSolver src
            return $ pushPathToDirections path
      let nePaths = filter (not . null) paths
      let selected =
            if null nePaths
              then []
              else minimumBy (comparing length) nePaths
      return selected
    tryMove2Boxes :: MonadState GameState m => [Point] -> [Point] -> m [Direction]
    tryMove2Boxes ss ts = do
      traceM [qm| {ss} -> {ts} |] -- [| [| g =<< (x >>= f) |] |]
      return []

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

findBoxDirections :: GameState -> Point -> [PD]
--     forall m. MonadState GameState m
findBoxDirections gs box =
  runIdentity $ do
    let m = gs ^. levelState . height
    let n = gs ^. levelState . width
    let w = gs ^. levelState . worker
    let paths =
          runST $ do
            hm <- HM.new
            let ctx = SolverContext hm m n
            flip evalStateT gs $ do
              let tryBuildPath src dst = do
                    moveSolver <- buildMoveSolver ctx dst [box]
                    accessible <- isAccessible dst
                    if accessible
                      then aStarFind moveSolver src
                      else return []
              mapM (\d -> tryBuildPath w (movePoint box $ opposite d)) [U, D, L, R]
    let augPaths = zip [U, D, L, R] paths
    let dirPoints = uncurry (PD box) . second pathToDirections <$> filter (not . null . snd) augPaths
    return dirPoints

--xyz :: IO ()
--xyz = do
--  putStrLn "A B C"
--  result <- runMaybeT $ do
--    maybeX <- Just . (== "1") <$> lift getLine
--    maybeY <- Just <$> lift getLine
--    x <- lift maybeX
--    y <- lift maybeY
--    lift $ putStrLn $ "You entered x = " <> show x <> " y = " <> show y
--  putStrLn [qm| result = {result}|]
findBoxDirections2 :: GameState -> (Point, Point) -> [PPD]
findBoxDirections2 gs (box1, box2) =
  runIdentity $ do
    let m = gs ^. levelState . height
    let n = gs ^. levelState . width
    let w = gs ^. levelState . worker
    return $ runST $ do
      hm <- HM.new
      let ctx = SolverContext hm m n
      flip evalStateT gs $ do
        let part0 = map (box1, , 0) [U, D, L, R]
        let part1 = map (box2, , 1) [U, D, L, R]
        let sources = part0 <> part1
        ppds' <-
          forM sources $ \(box, d, i) -> do
            let dst = movePoint box $ opposite d
            -- here runMaybeT :: MaybeT (StateT GameState m) PPD -> StateT GameState m (Maybe PPD)
            runMaybeT $ do
              _acc <- MaybeT $ accessibleToMaybe <$> isAccessible dst
              path <- MaybeT $ pathToMaybe <$> tryBuildPath ctx w dst
              let dirs = pathToDirections path
              return $ PPD box1 box2 d i dirs
        return $ catMaybes ppds'
  where
    tryBuildPath ::
         forall m. (PrimMonad m)
      => SolverContext (StateT GameState m)
      -> Point
      -> Point
      -> StateT GameState m [Point]
    tryBuildPath ctx src dst = do
      moveSolver <- buildMoveSolver ctx dst [box1, box2]
      aStarFind moveSolver src
    accessibleToMaybe p =
      if p
        then Just ()
        else Nothing
    pathToMaybe xs =
      if null xs
        then Just xs
        else Nothing

buildMoveSolver ::
     forall m n. (PrimMonad m, MonadState GameState n)
  => SolverContext m
  -> Point
  -> [Point]
  -> m (AStarSolver n Point)
buildMoveSolver ctx dst walls = do
  let m = ctx ^. cHeight
  let n = ctx ^. cWidth
  let nodesBound = m * n * 2
  return $
    AStarSolver
      { neighbors = neighbors
      , distance = distance
      , heuristic = heuristic
      , stopCond = stopCond
      , projection = point2kN n
      , injection = k2pointN n
      , nodesBound = nodesBound
      }
  where
    neighbors :: (MonadState GameState n) => Point -> n [Point]
    neighbors p0
          --isAccessible :: Point -> StateT GameState m Bool
     = do
      let isAccessible p =
            if p `elem` walls
              then return False
              else isEmptyOrGoal <$> getCell p
      let neighs = map (movePoint p0) [U, D, L, R]
      filterM isAccessible neighs
    distance np p0 = fromEnum (np /= p0)
    heuristic (Point i1 j1) =
      let Point i2 j2 = dst
       in return $ abs (i1 - i2) + abs (j1 - j2)
    stopCond = (==) dst

buildPushSolver ::
     forall m. PrimMonad m
  => SolverContext m
  -> PD
  -> m (AStarSolver (StateT GameState m) PD)
buildPushSolver ctx dst = do
  let m = ctx ^. cHeight
  let n = ctx ^. cWidth
  let nodesBound = m * n * 4
  return $
    AStarSolver
      { neighbors = neighbors
      , distance = distance
      , heuristic = heuristic
      , stopCond = stopCond
      , projection = pd2kN n
      , injection = k2pdN n
      , nodesBound = nodesBound
      }
  where
    neighbors (PD p0 d0 _ds0) = do
      let tryBuildPath src dst = do
            accessible <- isAccessible dst
            if accessible
              then pathToDirections <$> cachingFindPath ctx [p0] d0 src dst
              else return []
      -- cont is the "continue push in the direction d0" neighbor
      -- src is the position of the worker for the push
      -- directed is the same box but with changed push direction
      cont <- filterM (\(PD p _ _) -> isAccessible p) [PD (movePoint p0 d0) d0 [d0]]
      let src = movePoint p0 (opposite d0)
      let otherDirs = filter (/= d0) [U, D, L, R]
      paths <- mapM (\d -> PD p0 d <$> tryBuildPath src (movePoint p0 $ opposite d)) otherDirs
      (cont <>) <$> filterM (\(PD _ _ ds) -> (return . not . null) ds) paths
    heuristic (PD (Point i1 j1) d1 ds1) =
      let PD (Point i2 j2) d2 _ = dst
       in return $ abs (i1 - i2) + abs (j1 - j2) + fromEnum (d1 /= d2) + length ds1
    distance np p0 = fromEnum (np /= p0)
    stopCond (PD p _ _) =
      let PD t _ _ = dst
       in p == t

buildPushSolver2 ::
     forall m. PrimMonad m
  => SolverContext m
  -> (Point, Point)
  -> m (AStarSolver (StateT GameState m) PPD)
buildPushSolver2 ctx dst2 = do
  let m = ctx ^. cHeight
  let n = ctx ^. cWidth
  let nodesBound = 2 * m * n * m * n * 8
  return $
    AStarSolver
      { neighbors = neighbors ctx
      , distance = distance
      , heuristic = heuristic dst2
      , stopCond = stopCond dst2
      , projection = ppd2kMN m n
      , injection = k2ppdMN m n
      , nodesBound = nodesBound
      }
  where
    neighbors :: PrimMonad m => SolverContext m -> PPD -> StateT GameState m [PPD]
    neighbors ctx ppd = do
      let part0 = map (, 0) [U, D, L, R]
      let part1 = map (, 1) [U, D, L, R]
      let sources = part0 <> part1
      candidates <- mapM (uncurry (ppdNeighbor ctx ppd)) sources
      return $ catMaybes candidates
    -- valid for neighbors only
    distance (PPD p0 q0 _ _ _) (PPD p1 q1 _ _ _) = fromEnum (p0 /= p1) + fromEnum (q0 /= q1)
    stopCond dst (PPD p0 q0 _ _ _) = (p0, q0) == dst
    -- heuristic = undefined
    heuristic dst (PPD p0 q0 _d _i _dirs) = do
      let (p1, q1) = dst
      return $ distPP p1 p0 + distPP q1 q0

distPP :: Point -> Point -> Int
distPP (Point i1 j1) (Point i2 j2) = abs (i2 - i1) + abs (j2 - j1)

pdNeighbor :: PrimMonad m => SolverContext m -> PD -> Direction -> StateT GameState m (Maybe PD)
pdNeighbor ctx pd d = do
  let (PD p d0 _) = pd
  let walls = [p]
  let w = movePoint p (opposite d0) -- where worker is
  let p1 = movePoint p (opposite d)
  let p2 = movePoint p d
  accessible0 <- isAccessible p1
  accessible1 <- isAccessible p2
  if not (accessible0 && accessible1)
    then return Nothing
    else do
      points <- cachingFindPath ctx walls d w p1
      if null points
        then return Nothing
        else do
          let dirs = pathToDirections points <> [d]
          return $ Just $ PD p2 d dirs

-- PD Point Direction [Direction]
ppdNeighbor :: PrimMonad m => SolverContext m -> PPD -> Direction -> Int -> StateT GameState m (Maybe PPD)
ppdNeighbor ctx ppd d i = do
  let walls = [ppd ^. ppdFst, ppd ^. ppdSnd]
  let p = walls !! i
  -- invariant that the worker is
  let w = movePoint (ppd ^. ppdSelector) $ opposite (ppd ^. ppdDir)
  let p1 = movePoint p (opposite d)
  let p2 = movePoint p d
  accessible0 <- (p1 `notElem` walls &&) <$> isAccessible p1
  accessible1 <- (p2 `notElem` walls &&) <$> isAccessible p2
  if not (accessible0 && accessible1)
    then return Nothing
    else do
      points <- cachingFindPath ctx walls d w p1
      if null points
        then return Nothing
          --λ> ppd & ppdIdx .~ 0 & selector .~ p --> (0∙0 8∙4 R 0 [])
          --λ> ppd & ppdIdx .~ 1 & selector .~ p --> (7∙4 0∙0 R 1 [])
        else do
          let dirs = pathToDirections points <> [d]
          return $ Just $ ppd & ppdIdx .~ i & ppdSelector .~ p2 & ppdDir .~ d & ppdDirs .~ dirs

cachingFindPath ::
     PrimMonad m => SolverContext m -> [Point] -> Direction -> Point -> Point -> StateT GameState m [Point]
cachingFindPath ctx walls d src dst = do
  moveSolver <- lift $ buildMoveSolver ctx dst walls
  let pc = ctx ^. pathCache
  path' <- HM.lookup pc (walls, d, src, dst)
  case path' of
    Just path -> return path
    Nothing -> do
      path <- aStarFind moveSolver src
      HM.insert pc (walls, d, src, dst) path
      return path

isAccessible :: MonadState GameState m => Point -> m Bool
isAccessible p = isEmptyOrGoal <$> getCell p

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

point2kN :: Int -> Point -> Int
point2kN n (Point i j) = i * n + j

k2pointN :: Int -> Int -> Point
k2pointN n k = Point (k `div` n) (k `mod` n)

pd2kN :: Int -> PD -> Int
pd2kN n (PD (Point i j) d _) = (i * n + j) * 4 + fromIntegral (w8FromDirection d)

k2pdN :: Int -> Int -> PD
k2pdN n k =
  let kdir = k `mod` 4
      k4 = k `div` 4
   in PD (Point (k4 `div` n) (k4 `mod` n)) (w8ToDirection (fromIntegral kdir)) []

ppd2kMN :: Int -> Int -> PPD -> Int
ppd2kMN m n (PPD (Point i1 j1) (Point i2 j2) d idx _) =
  let k1 = i1 * n + j1
      k2 = i2 * n + j2
      mn = m * n
   in (k1 * mn + k2) * 8 + fromIntegral (w8FromDirection d) * 2 + idx

k2ppdMN :: Int -> Int -> Int -> PPD
k2ppdMN m n k =
  let (kpp, kdi) = k `divMod` 8
      (kd, ki) = kdi `divMod` 2
      mn = m * n
      (kpp1, kpp2) = kpp `divMod` mn
      p1 = Point (kpp1 `div` n) (kpp1 `mod` n)
      p2 = Point (kpp2 `div` n) (kpp2 `mod` n)
      d = w8ToDirection (fromIntegral kd)
      i = ki
   in PPD p1 p2 d i []

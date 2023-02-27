{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NumericUnderscores   #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE RecursiveDo          #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE FlexibleInstances    #-}

module Sokoban.Example where

import Prelude hiding (id)

import Control.Concurrent          (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.Async    (Async, async, wait)
import Control.Concurrent.STM      (TChan, TVar, atomically, newTChanIO, newTVarIO, readTChan,
                                    readTVar, writeTChan, writeTVar)
import Control.Exception.Base      (bracket)
import Control.Lens                ((%=), (%~), (&), (.~), (?~), (^.))
import Control.Lens.TH             (makeLenses)
import Control.Monad.Identity      (runIdentity)
import Control.Monad.State.Strict  (StateT, runStateT)
import Control.Monad.Writer.Strict (WriterT, execWriterT, runWriterT, tell)
import Sokoban.Keys                (keyLoop)
import Sokoban.Level               (Direction(..))
import Text.InterpolatedString.QM  (qm, qms)
import System.IO

import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Sokoban.Keys as K

-- double directed point, we can use this for box moves
data PPD =
  PPD
    { _ppdFst   :: Point
    , _ppdSnd   :: Point
    , _ppdDir   :: Direction
    , _ppdIdx   :: Int
    , _ppdDirs :: [Direction]
    }
  deriving (Eq, Generic, Hashable)

makeLenses ''PPD

ppdSelector :: Functor f => (Point -> f Point) -> PPD -> f PPD
ppdSelector f ppd@(PPD _ _ _ i _) = if i == 0 then ppdFst f ppd else ppdSnd f ppd

instance Ord PPD where
  compare = compare `on` (_ppdFst &&& _ppdSnd &&& _ppdDir &&& _ppdIdx)
  -- elegant variation for
  --   compare pd1 pd2 = compare
  --     (pd1 ^. pointFst, pd1 ^. pointSnd, pd1 ^. dirFst, pd1 ^. dirSnd)
  --     (pd2 ^. pointFst, pd2 ^. pointSnd, pd2 ^. dirFst, pd2 ^. dirSnd)

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
              let dirs = convPathToDirections path
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
      if not $ null xs
        then Just xs
        else Nothing


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
          let dirs = convPathToDirections points <> [d]
          return $ Just $ ppd & ppdIdx .~ i & ppdSelector .~ p2 & ppdDir .~ d & ppdDirs .~ dirs


convPushPathToDirections2 :: [PPD] -> [Direction]
convPushPathToDirections2 ppds = reverse $ foldl' (\acc ppd -> reverse (ppd ^. ppdDirs) <> acc) [] ppds

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


moveBoxesByWorkerCalc :: MonadState GameState m => [Point] -> [Point] -> m [Direction]
moveBoxesByWorkerCalc src dst =
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


data GameState =
  GameState
    { _xxx      :: Int
    , _yyy      :: Int
    , _txt      :: T.Text
    , _progress :: Int
    , _thread   :: Maybe ThreadId
    }
  deriving (Eq, Ord, Show)

-- use cases
-- 1. run animation (start animation, and then stop from the animation function)
-- 2. run animation, cancel (immediate jump to the final state)
-- 3. start calculation, show progress and cancel
-- 4. start calculation, show progress and at the finish, start the animation, cancel the animation
-- 5. start calculation, show progress and at the finish, start the animation and wait for its finish
data Message
  = MsgMoveStart Direction
  | MsgCalcStart Direction
  | MsgCalcFinish GameState
  | MsgAnimateStart Direction Int GameState
  | MsgAnimateStop GameState
  | MsgCancel
  | MsgTick
  deriving (Eq, Show)

data SoEvent
  = EvAnimateFinish GameState
  | Ev

data StateIO =
  StateIO
    { _msgChan  :: TChan Message
    , _taskChan :: TChan (Async Message)
    , _pidsVar  :: TVar [ThreadId]
    }
  deriving (Eq)

makeLenses ''GameState

makeLenses ''StateIO

x :: Async Message
x = undefined

-- bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
example :: IO ()
example = do
  sio <- StateIO <$> newTChanIO <*> newTChanIO <*> newTVarIO []
  bracket (setupAll sio) destroyAll $ \_ -> keyLoop (sio ^. msgChan) decodeKey -- blocks forever
  where
    setupAll :: StateIO -> IO ThreadId
    setupAll sio = do
      let gs = GameState 0 0 "" 0 Nothing
      hSetBuffering stdin NoBuffering
      hSetEcho stdin False
      -- hide cursor
      putStrLn "\ESC[?25l"
      forkIO $ gameLoop sio gs
    destroyAll :: ThreadId -> IO ()
    destroyAll gameLoopTid = do
      putStrLn "\ESC[?25h" -- show cursor
      killThread gameLoopTid

-- let (msgs, gs1) = step gs (MsgMove d)
-- forM_ (reverse msgs) $ \msg -> atomically $ writeTChan chan msg
-- background <- newTVarIO Nothing :: IO (Maybe ThreadId)
gameLoop :: StateIO -> GameState -> IO ()
gameLoop sio gs = do
  render gs
  let chan = sio ^. msgChan
  let pidsV = sio ^. pidsVar
  message <- atomically $ readTChan chan
  gs2 <-
    case message of
      MsgMoveStart d -> do
        atomically $ writeTChan chan MsgCancel
        atomically $ writeTChan chan (MsgCalcStart d)
        return gs
      MsgCalcStart d -> do
        pid <- forkIO $ progressLoop chan
        cid <- forkIO $ calculateProc chan gs d
        atomically $ writeTVar pidsV [pid, cid]
        return gs
      MsgCalcFinish gs1 -> do
        atomically (readTVar pidsV) >>= mapM_ killThread
        return gs1
      MsgAnimateStart d n gs2 -> do
        pid <- forkIO $ animateProc chan gs d n gs2
        atomically $ writeTVar pidsV [pid]
        return gs2
      MsgAnimateStop gs2 -> do
        atomically (readTVar pidsV) >>= mapM_ killThread
        return gs2
      MsgCancel -> do
        atomically (readTVar pidsV) >>= mapM_ killThread
        return gs
      MsgTick -> return (gs & txt %~ (<> "."))
  gameLoop sio gs2

-------------------------------------
animateProc :: TChan Message -> GameState -> Direction -> Int -> GameState -> IO ()
animateProc chan gs d n gs2 =
  if n > 0
    then do
      render gs
      threadDelay 1000000 -- 1 second
      animateProc chan (applyMove d gs) d (n - 1) gs2
    else do
      render gs2
      atomically $ writeTChan chan (MsgAnimateStop gs2)

calculateProc :: TChan Message -> GameState -> Direction -> IO ()
calculateProc chan gs1 d = do
  let n = 10
  let gs2 = last $ take n $ iterate (applyMove d) gs1
  threadDelay 10000000 -- simulate the calculation
  atomically $ do
    writeTChan chan (MsgCalcFinish gs2)
    writeTChan chan (MsgAnimateStart d n gs2)

progressLoop :: TChan Message -> IO ()
progressLoop chan = do
  threadDelay 1000000 -- 1 second
  atomically $ writeTChan chan MsgTick
  progressLoop chan

render :: GameState -> IO ()
render gs = T.putStrLn [qms|state: x={gs ^. xxx} y={gs ^. yyy}: {gs ^. progress}|]

type App m a = WriterT [Message] (StateT GameState m) a

step :: GameState -> Message -> ([Message], GameState)
step gs msg = runIdentity $ runStateT (execWriterT $ runStep msg) gs

runStep :: Monad m => Message -> App m ()
runStep msg =
  case msg of
    MsgMoveStart d -> move d
    _              -> return ()

decodeKey :: K.Key -> Maybe Message
decodeKey key =
  case key of
    K.Arrow d -> Just (MsgMoveStart d)
    K.Escape  -> Just MsgCancel
    _         -> Nothing

move :: Monad m => Direction -> App m ()
move d = tell [MsgCalcStart d]

applyMove :: Direction -> GameState -> GameState
applyMove d gs1 =
  case d of
    U -> gs1 & yyy %~ sub
    D -> gs1 & yyy %~ add
    L -> gs1 & xxx %~ add
    R -> gs1 & xxx %~ sub
  where
    add x = x + 1
    sub x = x - 1

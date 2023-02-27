{-# LANGUAGE BinaryLiterals        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE NumericUnderscores #-}

module Sokoban.Console where

import Prelude hiding (id)

import Debug                      (setDebugModeM)
import Control.Concurrent         (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.STM     (TChan, TVar, atomically, newTChanIO,
                                   newTVarIO, readTChan, readTVar, writeTChan, writeTVar, readTVarIO)
import Control.Exception          (finally)
import Control.Lens               (Lens', lens, use, (%=), (&), (.=), (.~), (^.))
import Control.Monad              (forM_, replicateM_, unless, when)
import Control.Monad.Primitive    (PrimMonad)
import Control.Monad.State.Strict (MonadState, StateT, execState, runState)
import Data.Maybe                 (fromMaybe, isJust)
import Data.Vector                ((!))
import Sokoban.Level              (Cell(..), Direction(..), LevelCollection(..), Point(..),
                                   isBox, isEmptyOrGoal, isWorker, levels, PD(..))
import Sokoban.Model              (AnimationMode(..), GameState(..), SolverContext(..),
                                   ViewState(..), animateRequired, animationMode, cells,
                                   clicks, destinations, direction, doClearScreen, doMove,
                                   eraseBoxes, getCell, height, id, initialLevelState, isComplete,
                                   levelState, levelState, message, moveCount, progress, pushCount,
                                   stats, step, undoIndex, undoMove, undoStack, viewState, width,
                                   _UndoItem, buildPushSolver)
import Sokoban.Parser             (parseLevels)
import Sokoban.Resources          (testCollection)
import Sokoban.Solver             (AStarSolver)
import System.Console.ANSI        (BlinkSpeed(SlowBlink), Color(..), ColorIntensity(..),
                                   ConsoleLayer(..), SGR(..), setSGR)
import System.Environment         (getArgs)
import System.IO                  (BufferMode(..), hSetBuffering, hSetEcho, stdin)
import System.IO.Unsafe           (unsafePerformIO)
import Text.InterpolatedString.QM (qms)

import qualified Data.HashMap.Mutable.Basic as HM
import qualified Data.HashSet               as S
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import qualified Sokoban.Model              as A (Action(..))
import qualified Sokoban.Keys               as K (Key(..), keyLoop)

animationTickDelay :: Int
animationTickDelay = 20 * 1000

animationTickDelayInUndoRedo :: Int
animationTickDelayInUndoRedo = 200

whenWith :: Monad m => a -> (a -> Bool) -> m a -> m a
whenWith a p runA =
  if p a
    then runA
    else return a

data Message
  = MsgKey K.Key
  | MsgTick


-- | run console game
run :: IO ()
run = do
  setDebugModeM False
  args <- getArgs
  gs <- buildGameState args
  chan <- newTChanIO :: IO (TChan Message)
  tvar <- newTVarIO Nothing :: IO (TVar (Maybe ThreadId))
  setupAll chan tvar gs `finally` destroyAll tvar
  where
    setupAll chan tvar gs = do
      setupScreen
      tid <- forkIO $ gameLoop chan gs
      atomically $ writeTVar tvar (Just tid)
      K.keyLoop chan (Just . MsgKey)
    destroyAll tvar = do
      Just tid <- atomically $ readTVar tvar
      killThread tid
      destroyScreen
    setupScreen = do
      hSetBuffering stdin NoBuffering
      hSetEcho stdin False
      clearScreen
      putStrLn "\ESC[?25l" -- hide cursor
       -- enable mouse capturing mode
      putStrLn "\ESC[?1000h"
      putStrLn "\ESC[?1015h"
      putStrLn "\ESC[?1006h"
    destroyScreen = do
      putStrLn "\ESC[?25h" -- show cursor
       -- disable mouse capturing mode
      putStrLn "\ESC[?1006l"
      putStrLn "\ESC[?1015l"
      putStrLn "\ESC[?1000l"


buildGameState :: [String] -> IO GameState
buildGameState args = do
  levelCollection <-
    if null args
      then return testCollection -- default
      else do
        let fileName = head args
        levels0 <- fromMaybe (error $ "Cannot parse file " <> fileName) . parseLevels <$> T.readFile fileName
        let levels = filter (isJust . initialLevelState) levels0 -- check invariants for each level as well
        when (null levels) $ error $ "File " <> fileName <> " contains no sokoban levels"
        return $
          LevelCollection
            {_title = T.pack fileName, _description = "", _email = "", _url = "", _copyright = "", _levels = levels}
  -- now we know, that the collection contains at least 1 valid level
  return $
    GameState
      { _collection = levelCollection
      , _index = 0
      , _levelState = fromMaybe (error "Impossible") $ initialLevelState $ head (levelCollection ^. levels)
      , _viewState =
          ViewState
            { _doClearScreen = False
            , _clicks = []
            , _destinations = S.empty
            , _animateRequired = False
            , _animationMode = AnimationDo
            , _message = "Controls: ← ↑ → ↓ R U I PgUp PgDn Mouse"
            , _progress = ""
            }
      }


convertKeyToAction :: K.Key -> A.Action
convertKeyToAction k = case k of
  K.Arrow dir    -> A.Move dir
  K.PageUp       -> A.PrevLevel
  K.PageDown     -> A.NextLevel
  K.Letter '['   -> A.PrevLevel
  K.Letter ']'   -> A.NextLevel
  K.Letter 'u'   -> A.Undo
  K.Letter 'i'   -> A.Redo
  K.Letter 'r'   -> A.Restart
  K.Letter 'd'   -> A.ToggleDebugMode
  K.Escape       -> A.Cancel
  K.MouseClick _ -> A.Cancel
  K.Letter _     -> A.Cancel


gameLoop :: TChan Message -> GameState -> IO ()
gameLoop chan gs0 = do
  unless False $ do
    moveCursorToOrigin
    render gs0
  msg <- atomically $ readTChan chan
  gs1 <-
    case msg of
      MsgKey (K.MouseClick click) -> 
        case interpretClick gs0 click of
          (Just _action@(A.MoveBoxesStart _src _dst), gs) ->
--            forM_ (gs0 ^. viewState . threadIds) killThread -- avoid leaking threads
--            tid1 <- forkIO $ progressLoop chan
--            tid2 <- forkIO $ calculate chan gs  
--            let gs' = gs & viewState . threadIds .~ [tid1, tid2]
--            return $ step gs' action
              return gs
          (Just action, gs) -> return $ step gs action
          (Nothing, gs)     -> return gs
      MsgKey K.Escape -> 
--      forM_ (gs0 ^. viewState . threadIds) killThread
        return gs0
      MsgKey key -> return $ step gs0 (convertKeyToAction key) 
--      CmdFinish dirs -> do
--        forM_ (gs0 ^. viewState . threadIds) killThread
--        return gs0
--      CmdAction action -> return $ step gs0 action
--      CmdTick -> return $ gs0 & viewState . progress %~ (<> ".")
  -- perform animation if needed
      MsgTick ->
        return gs0
  gs2 <-
    whenWith gs1 (^. (viewState . animateRequired)) $ do
      animate gs0 gs1
      return $ gs1 & viewState . animateRequired .~ False & viewState . animationMode .~ AnimationDo
  -- clear screen if needed
  gs3 <-
    whenWith gs2 (^. (viewState . doClearScreen)) $ do
      clearScreen
      return $ gs2 & viewState . doClearScreen .~ False
  gameLoop chan gs3

progressLoop :: TChan Message -> IO ()
progressLoop chan = do
  threadDelay 1_000_000 -- 1 second
  atomically $ writeTChan chan MsgTick
  progressLoop chan

calculate :: TChan Message -> GameState -> IO ()
calculate chan _gs = do
  putStrLn "Starting calculate ..."
  threadDelay 60_000_000 -- 60 second
  atomically $ writeTChan chan (MsgKey K.Escape)
  putStrLn "Finished calculate ..."

animate :: GameState -> GameState -> IO ()
animate gsFrom gsTo = do
  let undos = gsTo ^. levelState . undoStack
  let uidx = gsTo ^. levelState . undoIndex
  case gsTo ^. viewState . animationMode of
    AnimationDo -> do
      let dirs = map (^. direction) $ (undos !! uidx) ^. _UndoItem
      animateDo gsFrom dirs
    AnimationUndo -> do
      let diffs = reverse $ (undos !! (uidx - 1)) ^. _UndoItem
      animateUndo gsFrom diffs
    AnimationRedo -> do
      let dirs = map (^. direction) $ (undos !! uidx) ^. _UndoItem
      animateRedo gsFrom dirs
  where
    animateDo _ [] = return ()
    animateDo gs (dir:dirs) = do
      let gs2 = execState (doMove dir) gs
      moveCursorToOrigin
      render gs2
      threadDelay animationTickDelay
      animateDo gs2 dirs
    animateUndo _ [] = return ()
    animateUndo gs (diff:diffs) = do
      let gs2 = execState (undoMove diff) gs
      moveCursorToOrigin
      render gs2
      threadDelay animationTickDelayInUndoRedo
      animateUndo gs2 diffs
    animateRedo _ [] = return ()
    animateRedo gs (dir:dirs) = do
      let gs2 = execState (doMove dir) gs
      moveCursorToOrigin
      render gs2
      threadDelay animationTickDelayInUndoRedo
      animateRedo gs2 dirs


-- | The function converts the current click (coordinates, is_left_button_down)
-- | to the action. GameState is passed because the ViewState might be changed as well
interpretClick :: GameState -> (Point, Bool) -> (Maybe A.Action, GameState)
interpretClick gs click = runState runInterpretClick gs
  where
    runInterpretClick :: MonadState GameState m => m (Maybe A.Action)
    runInterpretClick = do
      complete <- use (levelState . isComplete)
      -- if complete then disable clicking
      if complete
        then return Nothing
        else case click of
               (_, True) -> return Nothing
               (click, False) -> do
                 let gather clks =
                       if click `elem` clks
                         then []
                         else click : clks
                 clickz <- gather <$> use (viewState . clicks)
                 cellz <- mapM getCell clickz
                 action <-
                   case (clickz, cellz) of
                     ([], []) -> do
                       viewState . clicks .= []
                       return Nothing
                     ([p0], [c0])
                       | isEmptyOrGoal c0 -> do
                         viewState . clicks .= []
                         return $ Just $ A.MoveWorker p0
                       | isWorker c0 -> do
                         viewState . clicks .= [p0]
                         return $ Just A.SelectWorker
                       | isBox c0 -> do
                         viewState . clicks .= [p0]
                         return $ Just $ A.SelectBox p0
                       | otherwise -> do
                         viewState . clicks .= []
                         return Nothing
                     ([p1, p0], [c1, c0])
                       | isWorker c0 && isDestination c1 -> do
                         viewState . clicks .= []
                         return $ Just $ A.MoveWorker p1
                       | isBox c0 && isDestination c1 -> do
                         viewState . clicks .= []
                         return $ Just $ A.MoveBoxesStart [p0] [p1]
                       | isBox c0 && isBox c1 -> do
                         viewState . clicks .= [p1, p0]
                         return $ Just $ A.SelectBox p1
                       | otherwise -> do
                         viewState . clicks .= []
                         return Nothing
                     (_, _) -> do
                       viewState . clicks .= []
                       viewState . destinations .= S.empty
                       return Nothing
                 case action of
                   Just (A.SelectBox _) -> return action
                   _ -> do
                     viewState . destinations .= S.empty
                     return action

isDestination :: Cell -> Bool
isDestination c =
  case c of
    Worker _       -> True
    WorkerOnGoal _ -> True
    Goal           -> True
    Empty          -> True
    _              -> False

clearScreen :: IO ()
clearScreen = putStrLn "\ESC[2J"

moveCursorToOrigin :: IO ()
moveCursorToOrigin = putStrLn "\ESC[1;1H"

render :: GameState -> IO ()
render gs = do
  let ls = gs ^. levelState
  let vs = gs ^. viewState
  let cs = ls ^. cells
  let m = ls ^. height
  let n = ls ^. width
  let dest = vs ^. destinations
  let points = [Point i j | i <- [0 .. m - 1], j <- [0 .. n - 1]]
  forM_ points $ \p -> do
    let Point i j = p
    let click = p `elem` (vs ^. clicks)
    let (char, color) = getCellSkin ((cs ! i) ! j) (S.member p dest) click
    let suffix =
          if j /= 0
            then " " <> [char]
            else "  " <> [char]
    if click
      then colorStr color True suffix
      else colorStr color False suffix
    when (j == n - 1) $ putStrLn ""
  let moves = ls ^. stats . moveCount
  let pushes = ls ^. stats . pushCount
  --  <> " " - this to avoid trailing stuff while undoing
  T.putStrLn $ [qms|Level: {ls ^. id} ({moves}, {pushes})|] <> " "
  T.putStrLn $ vs ^. message
  T.putStrLn $ vs ^. progress
  where
    colorStr :: Color -> Bool -> String -> IO ()
    colorStr color selected str = do
      setSGR $
        if selected
          then [SetColor Foreground Vivid color, SetBlinkSpeed SlowBlink]
          else [SetColor Foreground Vivid color]
      putStr str
      setSGR []
      putStr ""
    -- dst - is it the calculated destination for a box or worker
    -- click - is it selected by the mouse (true if selected)
    getCellSkin :: Cell -> Bool -> Bool -> (Char, Color)
    getCellSkin c dest click =
      case c of
        (Worker d) ->
          let color =
                if dest
                  then White
                  else Green
           in case d of
                U -> ('◒', color)
                D -> ('◓', color)
                L -> ('◑', color)
                R -> ('◐', color)
        (WorkerOnGoal d) ->
          let color =
                if dest
                  then White
                  else Red
           in case d of
                U -> ('◒', color)
                D -> ('◓', color)
                L -> ('◑', color)
                R -> ('◐', color)
        Wall -> ('▩', Blue)
        Empty ->
          case (dest, click) of
            (True, True)   -> ('꘎', White)
            (False, True)  -> ('꘎', White)
            (True, False)  -> ('·', White)
            (False, False) -> (' ', White)
        Goal ->
          case (dest, click) of
            (True, True)   -> ('✦', White)
            (False, True)  -> ('✧', Red)
            (True, False)  -> ('✦', White)
            (False, False) -> ('✧', Red)
        Box -> ('▩', Yellow)
        BoxOnGoal -> ('▩', Red)

------------------------------------------------------------------------------------------------------------------------
runTestPerf :: IO ()
runTestPerf = do
  clearScreen
  gs0 <- buildGameState []
  let gs1 = step gs0 (A.SelectBox (Point 11 2))
  let gs2 = step gs1 (A.SelectBox (Point 11 2))
  let gs3 = step gs2 (A.SelectBox (Point 11 2))
  let gs4 = step gs3 (A.MoveBoxesStart [Point 11 2] [Point 5 1])
  let gs5 = step gs4 (A.SelectBox (Point 7 3))
  let gs6 = step gs5 (A.SelectBox (Point 7 3))
  let gs7 = step gs6 (A.SelectBox (Point 7 3))
  let gs8 = step gs7 (A.SelectBox (Point 7 3))
  let gs9 = step gs8 (A.MoveBoxesStart [Point 7 3] [Point 5 2])
  render gs9

sources :: [(Direction, Integer)]
ctx :: SolverContext IO
gsa, gs0 :: GameState
ps1 :: AStarSolver (StateT GameState IO) PD
(gsa, gs0, ctx, sources, ps1) =
  unsafePerformIO $ do
    gs1 <- buildGameState []
    let gs0 = step gs1 (A.MoveWorker (Point 7 2))
    let gs = eraseBoxes [Point 11 2, Point 7 3] gs0
    ctx <- ctxGs gs
    ps1 <- buildPushSolver ctx (PD (Point 5 1) U [])
    let part0 = map (, 0) [U, D, L, R]
    let part1 = map (, 1) [U, D, L, R]
    let sources = part0 <> part1
    return (gs, gs0, ctx, sources, ps1)
  where
    ctxGs :: PrimMonad m => GameState -> m (SolverContext m)
    ctxGs gs = do
      hm <- HM.new
      let (m, n) = (gs ^. levelState . height, gs ^. levelState . width)
      return $ SolverContext hm m n

this :: Lens' a a
this = lens id (\_ v -> v)
  where
    id x = x

extractBoxes :: GameState -> [Point]
extractBoxes gs = execState extract []
  where
    extract = do
      let xs = gs ^. levelState . cells
      let m = gs ^. levelState . height
      let n = gs ^. levelState . width
      forM_ [0 .. m - 1] $ \i ->
        forM_ [0 .. n - 1] $ \j -> do
          let x = (xs ! i) ! j
          when (isBox x) $ this %= (Point i j :)

shared :: TVar Integer
{-# NOINLINE shared #-}
shared = unsafePerformIO $ newTVarIO 0

stmExample :: IO ()
stmExample = do
  before <- atomRead shared
  putStrLn $ "Before: " ++ show before
  _ <- forkIO $ replicateM_ 25 (dispVar shared >> milliSleep 20)
  _ <- forkIO $ replicateM_ 10 (appV (2 +) shared >> milliSleep 50)
  _ <- forkIO $ replicateM_ 20 (appV pred shared >> milliSleep 25)
  milliSleep 800
  after <- atomRead shared
  putStrLn $ "After: " ++ show after
  where
    milliSleep = threadDelay . (*) 1000

atomRead :: TVar a -> IO a
atomRead = readTVarIO

dispVar :: Show a => TVar a -> IO ()
dispVar x = atomRead x >>= print

appV :: (a -> a) -> TVar a -> IO ()
appV fn x = atomically $ readTVar x >>= writeTVar x . fn

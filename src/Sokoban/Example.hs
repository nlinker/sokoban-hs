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

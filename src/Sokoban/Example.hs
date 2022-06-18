{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE RecursiveDo #-}

module Sokoban.Example where

import Prelude hiding (id)

import Control.Concurrent          (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.STM      (TChan, TVar, atomically, newTChanIO, newTVarIO, readTChan,
                                    readTVar, writeTChan, writeTVar)
import Control.Exception           (finally)
import Control.Lens                ((%=), (%~), (&), (.~), (?~), (^.))
import Control.Lens.TH             (makeLenses)
import Control.Monad.Identity      (Identity, runIdentity)
import Control.Monad.Random.Strict (Rand, StdGen)
import Control.Monad.Reader        (ReaderT)
import Control.Monad.State.Strict  (MonadState, State, StateT, evalState, execState, execStateT,
                                    get, runState, runStateT)
import Control.Monad.Trans.Maybe   (MaybeT)
import Control.Monad.Writer.Strict (MonadWriter, WriterT, execWriterT, runWriterT, tell)
import Sokoban.Keys                (keyLoop)
import Sokoban.Level               (Direction(..))
import System.IO                   (BufferMode(..), hReady, hSetBuffering, hSetEcho, stdin)
import Text.InterpolatedString.QM  (qm, qms)

import           Data.List     (foldl')
import qualified Data.Text     as T
import qualified Data.Text.IO  as T
import           Debug.Trace   (traceM)
import qualified Sokoban.Keys  as K

import Data.IORef (newIORef, readIORef, writeIORef)

--import Reactive.Banana.Types ()
import Reactive.Banana.Combinators ()
import Reactive.Banana.Frameworks (newAddHandler, fromAddHandler, reactimate)
import Reactive.Banana (compile)
import Data.Char (toUpper)

echo1 = do
    (inputHandler, inputFire) <- newAddHandler
    compile $ do
        inputEvent <- fromAddHandler inputHandler
        -- turn all characters in the signal to upper case
        let inputEvent' = fmap (map toUpper) inputEvent
        let inputEventReaction = fmap putStrLn inputEvent' -- this has type `Event (IO ())
        reactimate inputEventReaction

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
  = MsgMove Direction
  | MsgCalcStart Direction
  | MsgCalcFinish GameState
  | MsgAnimateStart Direction Int GameState
  | MsgAnimateStop GameState
  | MsgCancel
  | MsgTick
  deriving (Eq, Show)

data Event
  = EvAnimateFinish GameState
  | Ev

data StateIO =
  StateIO
    { _channel :: TChan Message
    , _pidsVar :: TVar [ThreadId]
    }
  deriving (Eq)

makeLenses ''GameState

makeLenses ''StateIO

example :: IO ()
example = do
  let gs = GameState 0 0 "" 0 Nothing
  sio <- StateIO <$> newTChanIO <*> newTVarIO []
  gameTVar <- newTVarIO Nothing :: IO (TVar (Maybe ThreadId))
  setupAll sio gameTVar gs `finally` destroyAll gameTVar
  where
    setupAll :: StateIO -> TVar (Maybe ThreadId) -> GameState -> IO ()
    setupAll sio gameTVar gs = do
      setupScreen
      tid <- (Just <$>) <$> forkIO $ gameLoop sio gs
      atomically $ writeTVar gameTVar tid
      keyLoop (sio ^. channel) decodeKey -- blocks forever
    destroyAll :: TVar (Maybe ThreadId) -> IO ()
    destroyAll gameTVar = do
      Just tid <- atomically $ readTVar gameTVar
      killThread tid
      destroyScreen
    setupScreen = do
      hSetBuffering stdin NoBuffering
      hSetEcho stdin False
      -- hide cursor
      putStrLn "\ESC[?25l"
    destroyScreen = putStrLn "\ESC[?25h" -- show cursor

-- background <- newTVarIO Nothing :: IO (Maybe ThreadId)
gameLoop :: StateIO -> GameState -> IO ()
gameLoop sio gs = do
  render gs
  let chan = sio ^. channel
  let pidsV = sio ^. pidsVar
  message <- atomically $ readTChan chan
  gs2 <- case message of
    MsgMove d -> do
      -- let (msgs, gs1) = step gs (MsgMove d)
      -- forM_ (reverse msgs) $ \msg -> atomically $ writeTChan chan msg
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
      threadDelay 1_000_000 -- 1 second
      animateProc chan (applyMove d gs) d (n - 1) gs2
    else do
      render gs2
      atomically $ writeTChan chan (MsgAnimateStop gs2)

calculateProc :: TChan Message -> GameState -> Direction -> IO ()
calculateProc chan gs1 d = do
  let n = 10
  let gs2 = last $ take n $ iterate (applyMove d) gs1
  threadDelay 10_000_000 -- simulate the calculation
  atomically $ do
    writeTChan chan (MsgCalcFinish gs2)
    writeTChan chan (MsgAnimateStart d n gs2)

progressLoop :: TChan Message -> IO ()
progressLoop chan = do
  threadDelay 1_000_000 -- 1 second
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
    MsgMove d -> move d
    _         -> return ()

decodeKey :: K.Key -> Maybe Message
decodeKey key =
  case key of
    K.Arrow d -> Just (MsgMove d)
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

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Sokoban.Example where

import Prelude hiding (id)

import Control.Concurrent         (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.STM     (TChan, TVar, atomically, newTChanIO, newTVar, newTVarIO,
                                   readTVar, writeTChan, writeTVar)
import Control.Lens               ((%=), (%~), (&), (.~), (?~), (^.))
import Control.Lens.TH            (makeLenses)
import Text.InterpolatedString.QM (qms)

import Control.Monad.State.Strict (MonadState, evalState, execState)
import System.IO.Unsafe           (unsafePerformIO)

import           Control.Monad (forM_)
import           Data.IORef    (newIORef, readIORef, writeIORef)
import qualified Data.Text     as T
import qualified Data.Text.IO  as T
import           Sokoban.Level (Direction(..))

data GameState =
  GameState
    { _xxx      :: Int
    , _yyy      :: Int
    , _txt      :: T.Text
    , _commands :: [Command]
    , _thread   :: Maybe ThreadId
    }
  deriving (Eq, Ord, Show)

data Command
  = StartThread
  | StopThread
  | Progress
  deriving (Eq, Ord, Show)

data Action
  = Move Direction
  | Start
  | Stop
  deriving (Eq, Ord, Show)

makeLenses ''GameState

--backgroundThread :: TVar (Maybe ThreadId)
--{-# NOINLINE backgroundThread #-}
--backgroundThread = unsafePerformIO $ newTVarIO Nothing
example :: IO ()
example
  -- background <- newTVarIO Nothing :: IO (Maybe ThreadId)
 = do
  (channel :: TChan Command) <- newTChanIO
  let gs = GameState 0 0 "" [] Nothing
  gsRef <- newIORef gs
  render gs
  let actions = [Move L, Move U, Start]
  forM_ actions $ \a -> do
    gs <- readIORef gsRef
    let gs1 = step gs a
    gs2 <-
      case head $ gs1 ^. commands of
        StartThread -> do
          id <- forkIO $ backgroundThread channel
          return $ gs & thread ?~ id
        StopThread -> do
          let Just id = gs ^. thread
          killThread id
          return gs
    threadDelay 100_000 -- 100 milliseconds
    writeIORef gsRef gs2
    render gs2
  where
    backgroundThread :: TChan Command -> IO ()
    backgroundThread chan = do
      threadDelay 1000_000 -- 1 second
      atomically $ writeTChan chan Progress
      backgroundThread chan

applyV :: TVar a -> (a -> a) -> IO ()
applyV x fn = atomically $ readTVar x >>= writeTVar x . fn

step :: GameState -> Action -> GameState
step state action = (execState $ runStep action) state

runStep :: MonadState GameState m => Action -> m ()
runStep action =
  case action of
    Move d -> move d
    Start  -> start
    Stop   -> stop

move :: MonadState GameState m => Direction -> m ()
move d =
  case d of
    U -> yyy %= pred
    D -> yyy %= succ
    L -> xxx %= pred
    R -> xxx %= succ

start :: MonadState GameState m => m ()
start = undefined

stop :: MonadState GameState m => m ()
stop = undefined

render :: GameState -> IO ()
render gs = T.putStrLn [qms|state: {gs ^. xxx} {gs ^. yyy}: '{gs ^. txt}'|]

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Sokoban.Example where

import Prelude hiding (id)

import Control.Concurrent         (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.STM     (TChan, TVar, atomically, newTChanIO, newTVar, newTVarIO,
                                   readTChan, readTVar, writeTChan, writeTVar)
import Control.Lens               ((%=), (%~), (&), (.~), (?~), (^.))
import Control.Lens.TH            (makeLenses)
import Control.Monad              (forM_, when)
import Control.Monad.State.Strict (MonadState, evalState, execState)
import Data.IORef                 (newIORef, readIORef, writeIORef)
import Sokoban.Level              (Direction(..))
import System.IO                  (BufferMode(..), hReady, hSetBuffering, hSetEcho, stdin)
import System.IO.Unsafe           (unsafePerformIO)
import Text.InterpolatedString.QM (qms)

import           Control.Exception (finally)
import qualified Data.Text         as T
import qualified Data.Text.IO      as T

data GameState =
  GameState
    { _xxx      :: Int
    , _yyy      :: Int
    , _txt      :: T.Text
    , _progress :: Int
    , _thread   :: Maybe ThreadId
    , _commands :: [Command]
    }
  deriving (Eq, Ord, Show)

data Action
  = Move Direction
  | Start
  | Stop
  deriving (Eq, Ord, Show)

data Command
  = CmdTick
  | CmdAction Action
  deriving (Eq, Ord, Show)

makeLenses ''GameState

--backgroundThread :: TVar (Maybe ThreadId)
--{-# NOINLINE backgroundThread #-}
--backgroundThread = unsafePerformIO $ newTVarIO Nothing
example :: IO ()
example = do
  let gs = GameState 0 0 "" 0 Nothing []
  channel <- newTChanIO :: IO (TChan Command)
  tidTV <- newTVarIO Nothing :: IO (TVar (Maybe ThreadId))
  setupAll channel tidTV gs `finally` destroyAll tidTV
  where
    setupAll :: TChan Command -> TVar (Maybe ThreadId) -> GameState -> IO ()
    setupAll channel tidTV gs = do
      setupScreen
      tid <- forkIO $ gameLoop channel gs
      atomically $ writeTVar tidTV (Just tid)
      keyLoop channel
    destroyAll :: TVar (Maybe ThreadId) -> IO ()
    destroyAll tidTV = do
      Just tid <- atomically $ readTVar tidTV
      killThread tid
      destroyScreen
    setupScreen = do
      hSetBuffering stdin NoBuffering
      hSetEcho stdin False
      -- hide cursor
      putStrLn "\ESC[?25l"
    destroyScreen = putStrLn "\ESC[?25h" -- show cursor

keyLoop :: TChan Command -> IO ()
keyLoop chan = do
  key <- getKey
  case key of
    "\ESC[A" -> atomically $ writeTChan chan (CmdAction (Move U))
    "\ESC[B" -> atomically $ writeTChan chan (CmdAction (Move D))
    "\ESC[C" -> atomically $ writeTChan chan (CmdAction (Move L))
    "\ESC[D" -> atomically $ writeTChan chan (CmdAction (Move R))
    "s"      -> atomically $ writeTChan chan (CmdAction Start)
    "\ESC"   -> atomically $ writeTChan chan (CmdAction Stop)
    _        -> pure ()
  keyLoop chan

progressLoop :: TChan Command -> IO ()
progressLoop chan = do
  threadDelay 1_000_000 -- 1 second
  atomically $ writeTChan chan CmdTick
  progressLoop chan

-- background <- newTVarIO Nothing :: IO (Maybe ThreadId)
gameLoop :: TChan Command -> GameState -> IO ()
gameLoop chan gs = do
  render gs
  cmd <- atomically $ readTChan chan
  gs1 <- case cmd of
          CmdAction Start -> do
            tid <- forkIO $ progressLoop chan
            return $ gs & thread ?~ tid
          CmdAction Stop ->
            case gs ^. thread of
              Just tid -> do
                killThread tid
                return $ gs & thread .~ Nothing & progress .~ 0
              Nothing -> return gs
          CmdAction action -> return $ step gs action
          CmdTick -> return $ gs & progress %~ succ
  gameLoop chan gs1

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
    L -> xxx %= succ
    R -> xxx %= pred

start :: MonadState GameState m => m ()
start = commands %= (CmdAction Start :)

stop :: MonadState GameState m => m ()
stop = commands %= (CmdAction Stop :)

render :: GameState -> IO ()
render gs = T.putStrLn [qms|state: x={gs ^. xxx} y={gs ^. yyy}: {gs ^. progress} {gs ^. commands}|]

getKey :: IO String
getKey = reverse <$> getKey' ""
  where
    getKey' chars = do
      char <- getChar
      more <- hReady stdin
      (if more
         then getKey'
         else return)
        (char : chars)

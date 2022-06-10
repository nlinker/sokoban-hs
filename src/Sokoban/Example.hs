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
                                   readTVar, writeTChan, writeTVar)
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
example = do
  let gs = GameState 0 0 "" 0 [] Nothing
  (do setupScreen
      gameLoop gs) `finally`
    destroyScreen
  where
    setupScreen = do
      hSetBuffering stdin NoBuffering
      hSetEcho stdin False
      -- hide cursor
      putStrLn "\ESC[?25l"
    destroyScreen = putStrLn "\ESC[?25h" -- show cursor

-- (channel :: TChan Command) <- newTChanIO
-- background <- newTVarIO Nothing :: IO (Maybe ThreadId)
--    backgroundThread :: TChan Command -> IO ()
--    backgroundThread chan = do
--      threadDelay 1000_000 -- 1 second
--      atomically $ writeTChan chan Progress
--      backgroundThread chan
gameLoop :: GameState -> IO ()
gameLoop gs = do
  render gs
  key <- getKey
  when True $ do
    let gs1 =
          case key of
            "\ESC[A" -> step gs (Move U)
            "\ESC[B" -> step gs (Move D)
            "\ESC[C" -> step gs (Move L)
            "\ESC[D" -> step gs (Move R)
            "s"      -> step gs Start
            "\ESC"   -> step gs Stop
            _        -> gs
    gameLoop gs1

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
    L -> xxx %= succ
    R -> xxx %= pred

start :: MonadState GameState m => m ()
start = undefined

stop :: MonadState GameState m => m ()
stop = undefined

render :: GameState -> IO ()
render gs = T.putStrLn [qms|state: x={gs ^. xxx} y={gs ^. yyy}: '{gs ^. txt}'|]

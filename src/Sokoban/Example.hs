{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE Rank2Types #-}

module Sokoban.Example where

import Prelude hiding (id)

import Control.Concurrent         (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.STM     (TChan, TVar, atomically, newTChanIO, newTVarIO,
                                   readTChan, readTVar, writeTChan, writeTVar)
import Control.Lens               ((%=), (%~), (&), (.~), (?~), (^.))
import Control.Lens.TH            (makeLenses)
import Control.Monad.State.Strict (MonadState, evalState, execState, execStateT, StateT, State, runState, runStateT)
import Sokoban.Level              (Direction(..))
import System.IO                  (BufferMode(..), hReady, hSetBuffering, hSetEcho, stdin)
import Text.InterpolatedString.QM (qms)
import Control.Exception          (finally)
import Sokoban.Keys (keyLoop)
import Control.Monad.Writer.Strict (WriterT, tell, runWriterT, execWriterT)
import Control.Monad.Random.Strict (StdGen, Rand)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Identity (Identity, runIdentity)

import qualified Data.Text         as T
import qualified Data.Text.IO      as T
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

data Action
  = Move Direction
  | Start
  | Stop
  deriving (Eq, Ord, Show)

data Message
  = MsgKey K.Key
  | MsgTick
  deriving (Eq, Show)

data Command = Command
  deriving (Eq, Ord, Show)

makeLenses ''GameState

--backgroundThread :: TVar (Maybe ThreadId)
--{-# NOINLINE backgroundThread #-}
--backgroundThread = unsafePerformIO $ newTVarIO Nothing
example :: IO ()
example = do
  let gs = GameState 0 0 "" 0 Nothing
  channel <- newTChanIO :: IO (TChan Message)
  tidTV <- newTVarIO Nothing :: IO (TVar (Maybe ThreadId))
  setupAll channel tidTV gs `finally` destroyAll tidTV
  where
    setupAll :: TChan Message -> TVar (Maybe ThreadId) -> GameState -> IO ()
    setupAll channel tidTV gs = do
      setupScreen
      tid <- forkIO $ gameLoop channel gs
      atomically $ writeTVar tidTV (Just tid)
      keyLoop MsgKey channel
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

progressLoop :: TChan Message -> IO ()
progressLoop chan = do
  threadDelay 1_000_000 -- 1 second
  atomically $ writeTChan chan MsgTick
  progressLoop chan

-- background <- newTVarIO Nothing :: IO (Maybe ThreadId)
gameLoop :: TChan Message -> GameState -> IO ()
gameLoop chan gs = do
  render gs
  message <- atomically $ readTChan chan
  gs1 <- case message of
          MsgKey (K.Letter 's') -> do
            tid <- forkIO $ progressLoop chan
            return $ gs & thread ?~ tid
          MsgKey K.Escape ->
            case gs ^. thread of
              Just tid -> do
                killThread tid
                return $ gs & thread .~ Nothing & progress .~ 0
              Nothing -> return gs
          MsgKey (K.Arrow dir) -> return $ step gs (dirToAction dir)
          MsgKey _ -> return gs
          MsgTick -> return $ gs & progress %~ succ
  gameLoop chan gs1

dirToAction :: Direction -> Action
dirToAction dir = case dir of
    U -> Move dir
    D -> Move dir
    L -> Move dir
    R -> Move dir

data Env = Env

type SolverT a = MaybeT
                    (WriterT [Command]
                        (ReaderT Env
                            (StateT GameState
                                (Rand StdGen)))) a

type App m a = WriterT [Command] (StateT GameState m) a

step :: GameState -> Action -> GameState
step gs action = runIdentity $ do
  let app = runStep action :: App Identity ()
  let (cmds, gs2) = runIdentity $ runStateT (execWriterT app) gs 
  return gs2          

runStep :: Monad m => Action -> App m ()
runStep action =
  case action of
    Move d -> move d
    Start  -> start
    Stop   -> stop

move :: Monad m => Direction -> App m ()
move d =
  case d of 
    U -> yyy %= pred
    D -> yyy %= succ
    L -> xxx %= succ
    R -> xxx %= pred

start :: Monad m => App m ()
start = tell [Command]

stop :: Monad m => App m ()
stop = tell [Command]

render :: GameState -> IO ()
render gs = T.putStrLn [qms|state: x={gs ^. xxx} y={gs ^. yyy}: {gs ^. progress}|]

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

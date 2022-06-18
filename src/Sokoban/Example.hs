{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Sokoban.Example where

import Prelude hiding (id)

import Control.Concurrent          (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.Async    (Async, async, wait)
import Control.Concurrent.STM      (TChan, TVar, atomically, newTChanIO, newTVarIO, readTChan,
                                    readTVar, writeTChan, writeTVar)
import Control.Exception.Base      (bracket)
import Control.Lens                ((%=), (%~), (&), (.~), (?~), (^.))
import Control.Lens.TH             (makeLenses)
import Control.Monad.Identity      (Identity, runIdentity)
import Control.Monad.Random.Strict (Rand, StdGen)
import Control.Monad.Reader        (ReaderT)
import Control.Monad.State.Strict  (MonadState, State, StateT, evalState, execState, execStateT,
                                    get, runState, runStateT)
import Control.Monad.Trans.Maybe   (MaybeT)
import Control.Monad.Writer.Strict (MonadWriter, WriterT, execWriterT, runWriterT, tell)
import Data.Char                   (toUpper)
import Data.IORef                  (newIORef, readIORef, writeIORef)
import Data.List                   (foldl')
import Debug.Trace                 (traceM)
import Reactive.Banana             (compile)
import Reactive.Banana.Combinators ()
import Reactive.Banana.Frameworks  (EventNetwork(..), MomentIO, fromAddHandler, newAddHandler,
                                    reactimate)
import Sokoban.Keys                (keyLoop)
import Sokoban.Level               (Direction(..))
import System.IO                   (BufferMode(..), hReady, hSetBuffering, hSetEcho, stdin)
import Text.InterpolatedString.QM  (qm, qms)

import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Sokoban.Keys as K

import Control.Monad (when)
import Data.IORef
import Data.List     (nub)
import Data.Maybe    (fromJust, isJust)
import Debug.Trace
import System.IO
import System.Random

import Reactive.Banana
import Reactive.Banana.Frameworks

cmdLine :: IO ()
cmdLine = do
  displayHelpMessage
  sources <- (,) <$> newAddHandler <*> newAddHandler
  network <- setupNetwork sources
  actuate network
  eventLoop sources network

displayHelpMessage :: IO ()
displayHelpMessage =
  mapM_ putStrLn
  [ "Commands are:"
  , "   count   - send counter event"
  , "   pause   - pause event network"
  , "   actuate - actuate event network"
  , "   quit    - quit the program"
  , ""
  ]

-- Read commands and fire corresponding events.
eventLoop :: (EventSource (), EventSource EventNetwork) -> EventNetwork -> IO ()
eventLoop (escounter, espause) network = loop
  where
    loop = do
      putStr "> "
      hFlush stdout
      s <- getLine
      case s of
        "c" -> fire escounter ()
        "p" -> fire espause network
        "a" -> actuate network
        "q" -> return ()
        _   -> putStrLn $ s ++ " - unknown command"
      when (s /= "q") loop

{-----------------------------------------------------------------------------
    Event sources
------------------------------------------------------------------------------}
-- Event Sources - allows you to register event handlers
-- Your GUI framework should provide something like this for you
type EventSource a = (AddHandler a, a -> IO ())

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

{-----------------------------------------------------------------------------
    Program logic
------------------------------------------------------------------------------}
-- Set up the program logic in terms of events and behaviors.
setupNetwork :: (EventSource (), EventSource EventNetwork) -> IO EventNetwork
setupNetwork (escounter, espause) =
  compile $ do
    ecounter <- fromAddHandler (addHandler escounter)
    epause <- fromAddHandler (addHandler espause)
    ecount <- accumE 0 $ (+ 1) <$ ecounter
    reactimate $ fmap print ecount
    reactimate $ fmap pause epause

{-
echo1 :: IO Reactive.Banana.Frameworks.EventNetwork
echo1 = do
  (inputHandler, inputFire) <- newAddHandler
  compile $ do
    inputEvent <- fromAddHandler inputHandler
        -- turn all characters in the signal to upper case
    let inputEvent' = fmap (map toUpper) inputEvent
    let inputEventReaction = fmap putStrLn inputEvent' -- this has type `Event (IO ())
    reactimate inputEventReaction
-}
async1 :: IO ()
async1 = do
  a1 <-
    async $ do
      putStrLn "Start a1"
      threadDelay 3000000 -- 3 second
      putStrLn "End a1"
      return 33
  putStrLn "Run next task"
  a2 <-
    async $ do
      putStrLn "Start a2"
      threadDelay 2000000 -- 2 second
      putStrLn "End a2"
      return 22
  x1 <- wait a1
  x2 <- wait a2
  putStrLn $ "x1 + x2 = " ++ show (x1 + x2)

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

data Event
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

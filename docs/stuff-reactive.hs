{-# LANGUAGE TypeSynonymInstances #-}
import Data.IORef                  (newIORef, readIORef, writeIORef)
import Data.List                   (foldl')
import Debug.Trace                 (traceM)
import System.IO                   (BufferMode(..), hReady, hSetBuffering, hSetEcho, stdin)
import Control.Monad (when)
import Data.IORef
import Data.List     (nub)
import Data.Maybe    (fromJust, isJust)
import Debug.Trace
import System.IO
import System.Random

import Reactive.Banana             (compile)
import Reactive.Banana.Combinators ()
import Reactive.Banana.Frameworks  (EventNetwork(..), MomentIO, fromAddHandler, newAddHandler, reactimate)
import Reactive.Banana
import Reactive.Banana.Frameworks

-- add to stack.yaml
-- reactive-banana-1.2.1.0

-- Event Sources - allows you to register event handlers
-- Your GUI framework should provide something like this for you
type EventSource a = (AddHandler a, a -> IO ())

getAddHandler :: EventSource a -> AddHandler a
getAddHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

cmdLine :: IO ()
cmdLine = do
  -- newAddHandler :: IO (AddHandler a, Handler a)
  -- newAddHandler :: Control.Event.Handler
  -- setupNetwork :: (EventSource (), EventSource EventNetwork) -> IO EventNetwork
  -- sources :: ((AddHandler (), Handler ()), (AddHandler EventNetwork, Handler EventNetwork))
  -- actuate :: EventNetwork -> IO ()
  -- eventLoop :: (EventSource (), EventSource EventNetwork) -> EventNetwork -> IO ()
  chan <- newTChanIO :: IO (TChan Message)
  fireEs <- newAddHandler
  network <- setupNetwork fireEs
  actuate network
  eventLoop chan fireEs network

-- Read commands and fire corresponding events.
eventLoop :: TChan Message -> EventSource Direction -> EventNetwork -> IO ()
eventLoop chan fireEs network = loop
  where
    loop = do
      message <- atomically $ readTChan chan
      _result <- case message of
        MsgMoveStart dir         -> fire fireEs dir
        MsgCancel                -> undefined
        MsgTick                  -> undefined
      loop

-- Set up the program logic in terms of events and behaviors.
setupNetwork :: EventSource Message -> IO EventNetwork
setupNetwork fireEs =
  compile $ do
    (ecounter :: Event ()) <- fromAddHandler (getAddHandler fireEs)
    (epause :: Event EventNetwork) <- fromAddHandler (getAddHandler pauseEs)
    let fe :: Event (Integer -> Integer)
        fe = (+ 1) <$ ecounter
    (ecount :: Event Integer) <- accumE 0 fe
    reactimate $ fmap print ecount
    reactimate $ fmap pause epause

kkk :: Int -> [[Char]]
kkk c = [replicate c ')']

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

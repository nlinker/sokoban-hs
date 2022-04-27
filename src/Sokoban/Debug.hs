{-# LANGUAGE Strict #-}

module Sokoban.Debug where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import System.IO.Unsafe        (unsafePerformIO)

-- (setDebugModeM, getDebugModeM)

debugMode :: MVar Bool
{-# NOINLINE debugMode #-}
debugMode = unsafePerformIO $ newMVar False

setDebugModeM :: Applicative f => Bool -> f ()
{-# NOINLINE setDebugModeM #-}
setDebugModeM mode =
  unsafePerformIO $ do
    modifyMVar_ debugMode (\_ -> return mode)
    return $ pure ()

getDebugModeM :: Applicative f => f Bool
{-# NOINLINE getDebugModeM #-}
getDebugModeM =
  let dm = unsafePerformIO $ readMVar debugMode
   in pure dm

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE Strict        #-}

module Debug where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import System.IO.Unsafe        (unsafePerformIO)

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

-- trace to file, not console
{-# NOINLINE traceFM #-}
traceFM :: Applicative f => String -> f ()
traceFM msg = pure $ traceF msg ()
  where
    file = "trace.log" :: FilePath
    traceF :: String -> a -> a
    traceF msg expr =
      unsafePerformIO $ do
        appendFile file (msg <> "\n")
        return expr

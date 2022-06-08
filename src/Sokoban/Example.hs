{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TemplateHaskell  #-}

module Sokoban.Example where

import Control.Concurrent         (forkIO, threadDelay)
import Control.Concurrent.STM     (TVar, atomically, newTVar, newTVarIO, readTVar, writeTVar)
import Control.Lens               ((^.))
import Control.Lens.TH            (makeLenses)
import Text.InterpolatedString.QM (qms)

import Control.Monad.State.Strict (MonadState, evalState, execState)
import System.IO.Unsafe           (unsafePerformIO)

import qualified Data.Text    as T
import qualified Data.Text.IO as T

data GameState =
  GameState
    { _xxx :: Int
    , _yyy :: Int
    , _txt :: T.Text
    }
  deriving (Eq, Ord, Show)

data Action
  = Mark Int
  | Start
  | Stop

makeLenses ''GameState

example :: IO ()
example = do
  let gs0 = GameState 0 0 ""
  render gs0

step :: GameState -> Action -> GameState
step state action = (execState $ runStep action) state

runStep :: MonadState GameState m => Action -> m ()
runStep action =
  case action of
    Mark a -> mark
    Start  -> startState
    Stop   -> stopState

mark :: MonadState GameState m => m ()
mark = undefined

startState :: MonadState GameState m => m ()
startState = undefined

stopState :: MonadState GameState m => m ()
stopState = undefined

shared :: TVar Integer
{-# NOINLINE shared #-}
shared = unsafePerformIO $ newTVarIO 0

render :: GameState -> IO ()
render gs = T.putStrLn [qms|state: {gs ^. xxx} {gs ^. yyy}: '{gs ^. txt}'|]

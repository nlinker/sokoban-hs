{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Sokoban.Console where

import Prelude hiding (id)

import Control.Concurrent     (threadDelay)
import Control.Exception      (finally)
import Control.Lens           (use, (&), (.=), (.~), (^.))
import Control.Monad          (forM_, when)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State    (MonadState, evalState, execState, runState)
import Data.Char              (isDigit)
import Data.List              (isSuffixOf, stripPrefix)
import Data.Maybe             (fromMaybe, isJust)
import Data.Vector            ((!))
import Sokoban.Level          (Cell(..), Direction(..), LevelCollection(..), Point(..), deriveDir,
                               isBox, isEmptyOrGoal, isWorker, levels)
import Sokoban.Model          (GameState(..), ViewState(..), animateForward, animateRequired, cells,
                               clicks, destinations, direction, doClearScreen, doMove, getCell,
                               height, id, initial, levelState, levelState, message, step,
                               undoIndex, undoMove, undoStack, viewState, width, worker, _UndoItem)
import Sokoban.Parser         (parseLevels, splitWith)
import Sokoban.Resources      (yoshiroAutoCollection)
import Sokoban.Solver         (aStarFind)
import System.Console.ANSI    (BlinkSpeed(SlowBlink), Color(..), ColorIntensity(..),
                               ConsoleLayer(..), SGR(..), setSGR)
import System.Environment     (getArgs)
import System.IO              (BufferMode(..), hReady, hSetBuffering, hSetEcho, stdin)
import Text.Read              (readMaybe)

import qualified Data.HashSet  as S
import qualified Data.Text     as T
import qualified Data.Text.IO  as T
import qualified Sokoban.Model as A (Action(..))

whenWith :: Monad m => a -> (a -> Bool) -> m a -> m a
whenWith a p runA =
  if p a
    then runA
    else return a

-- | run console gane
run :: IO ()
run =
  do args <- getArgs
     gameState <- buildGameState args
     do setupScreen
        gameLoop gameState
     `finally` destroyScreen
  where
    setupScreen = do
      hSetBuffering stdin NoBuffering
      hSetEcho stdin False
      clearScreen
        -- hide cursor
      putStrLn "\ESC[?25l"
       -- enable mouse capturing mode
      putStrLn "\ESC[?1000h"
      putStrLn "\ESC[?1015h"
      putStrLn "\ESC[?1006h"
    destroyScreen = do
      putStrLn "\ESC[?25h" -- show cursor
       -- disable mouse capturing mode
      putStrLn "\ESC[?1006l"
      putStrLn "\ESC[?1015l"
      putStrLn "\ESC[?1000l"

buildGameState :: [String] -> IO GameState
buildGameState args = do
  levelCollection <-
    if null args
      then return yoshiroAutoCollection -- default
      else do
        let fileName = head args
        levels0 <- fromMaybe (error $ "Cannot parse file " <> fileName) . parseLevels <$> T.readFile fileName
        let levels = filter (isJust . initial) levels0 -- check invariants for each level as well
        when (null levels) $ error $ "File " <> fileName <> " contains no sokoban levels"
        return $
          LevelCollection
            {_title = T.pack fileName, _description = "", _email = "", _url = "", _copyright = "", _levels = levels}
  -- now we know, that the collection contains at least 1 valid level
  return $
    GameState
      { _collection = levelCollection
      , _index = 0
      , _levelState = fromMaybe (error "Impossible") $ initial $ head (levelCollection ^. levels)
      , _viewState = ViewState False [] S.empty False False
      }

gameLoop :: GameState -> IO ()
gameLoop gs0
  -- if we need to draw multiple
 = do
  moveCursorToOrigin
  render gs0
  key <- getKey
  when (key /= "\ESC") $ do
    let gs1 =
          case key of
            "\ESC[A" -> step gs0 A.Up
            "\ESC[B" -> step gs0 A.Down
            "\ESC[C" -> step gs0 A.Right
            "\ESC[D" -> step gs0 A.Left
            "u" -> step gs0 A.Undo
            "i" -> step gs0 A.Redo
            "r" -> step gs0 A.Restart
            "\ESC[5~" -> step gs0 A.PrevLevel
            "\ESC[6~" -> step gs0 A.NextLevel
            "d" -> step gs0 A.Debug
            _ ->
              case interpretClick gs0 key of
                (Just action, gs) -> step gs action
                (Nothing, gs)     -> gs
    -- perform animation if needed
    gs2 <-
      whenWith gs1 (^. (viewState . animateRequired)) $ do
        animate gs0 gs1
        return $ gs1 
          & viewState . animateRequired .~ False 
          & viewState . animateForward .~ False
    -- clear screen if needed
    gs3 <-
      whenWith gs2 (^. (viewState . doClearScreen)) $ do
        clearScreen
        return $ gs2 & viewState . doClearScreen .~ False
    gameLoop gs3

animate :: GameState -> GameState -> IO ()
animate gsFrom gsTo = do
  let undos = gsTo ^. levelState . undoStack
  let uidx = gsTo ^. levelState . undoIndex
  if gsTo ^. viewState . animateForward
      -- when (0 < uidx && uidx <= length undos) $ do
    then when True $ do
           let dirs = map (^. direction) $ (undos !! uidx) ^. _UndoItem
           animateDo gsFrom dirs
      -- when (0 <= uidx && uidx < length undos) $ do
    else when True $ do
           let diffs = reverse $ (undos !! (uidx - 1)) ^. _UndoItem
           animateUndo gsFrom diffs
  where
    animateDo _ [] = return ()
    animateDo gs (dir:dirs) = do
      let gs2 = execState (doMove dir) gs
      moveCursorToOrigin
      render gs2
      threadDelay (50 * 1000)
      animateDo gs2 dirs
    animateUndo _ [] = return ()
    animateUndo gs (diff:diffs) = do
      let gs2 = execState (undoMove diff) gs
      moveCursorToOrigin
      render gs2
      threadDelay (50 * 1000)
      animateUndo gs2 diffs

interpretClick :: GameState -> String -> (Maybe A.Action, GameState)
interpretClick gs key = runState runInterpretClick gs
  where
    runInterpretClick :: MonadState GameState m => m (Maybe A.Action)
    runInterpretClick =
      case extractMouseClick key of
        Nothing -> return Nothing
        Just (_, True) -> return Nothing
        Just (click, False) -> do
          clickz <- (click :) <$> use (viewState . clicks)
          cellz <- mapM getCell clickz
          case (clickz, cellz) of
            ([], []) -> return Nothing
            ([p0], [c0])
              | isEmptyOrGoal c0 -> do
                viewState . clicks .= []
                return $ Just $ A.MoveWorker p0
              | isWorker c0 -> do
                viewState . clicks .= [p0]
                return Nothing
              | isBox c0 -> do
                viewState . clicks .= [p0]
                return Nothing
              | otherwise -> do
                viewState . clicks .= []
                return Nothing
            ([p1, p0], [c1, c0])
              | isWorker c0 && isDestination c1 -> do
                viewState . clicks .= []
                return $ Just $ A.MoveWorker p1
              | isBox c0 && isDestination c1 -> do
                viewState . clicks .= []
                return $ Just $ A.MoveBoxes [p0] [p1]
              | otherwise -> do
                viewState . clicks .= [] -- reset tracking clicks
                return Nothing
            _ -> do
              viewState . clicks .= [] -- reset tracking clicks
              viewState . destinations .= S.empty
              return Nothing

isDestination :: Cell -> Bool
isDestination c =
  case c of
    Worker _       -> True
    WorkerOnGoal _ -> True
    Goal           -> True
    Empty          -> True
    _              -> False

extractMouseClick :: String -> Maybe (Point, Bool)
extractMouseClick key = do
  rest <- stripPrefix "\ESC[<0;" key
  -- expected input in the form "\ESC[<0;2;3M" or "\ESC[<0;2;3m" ("m" is button up)
  let lbmDown = "M" `isSuffixOf` rest
  case readMaybe <$> splitWith isDigit rest :: [Maybe Int] of
    [Just x, Just y] -> Just (Point (y - 2) ((x - 1) `div` 2), lbmDown)
    _                -> Nothing

showInMessage :: Show a => GameState -> a -> GameState
showInMessage gs x =
  let nm = T.length $ gs ^. levelState . message
      msg1 = "action = " <> show x
   in gs & levelState . message .~ T.pack (msg1 <> replicate (nm - length msg1) ' ')

clearScreen :: IO ()
clearScreen = putStrLn "\ESC[2J"

moveCursorToOrigin :: IO ()
moveCursorToOrigin = putStrLn "\ESC[1;1H"

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

render :: GameState -> IO ()
render gs = do
  let ls = gs ^. levelState
  let cs = ls ^. cells
  let m = ls ^. height
  let n = ls ^. width
  let points = [Point i j | i <- [0 .. m - 1], j <- [0 .. n - 1]]
  forM_ points $ \p -> do
    let Point i j = p
    let (char, color) = getCellSkin $ (cs ! i) ! j
    colorStr color False $
      if j /= 0
        then " " ++ [char]
        else [char]
    when (j == n - 1) $ putStrLn ""
  T.putStrLn $ "Level: " <> ls ^. id
  T.putStrLn $ ls ^. message
  where
    colorStr :: Color -> Bool -> String -> IO ()
    colorStr color blink str = do
      setSGR $
        if blink
          then [SetColor Foreground Vivid color, SetColor Background Dull Black, SetBlinkSpeed SlowBlink]
          else [SetColor Foreground Vivid color, SetColor Background Dull Black]
      putStr str
      setSGR []
      putStr ""
    getCellSkin :: Cell -> (Char, Color)
    getCellSkin c =
      case c of
        (Worker d) ->
          case d of
            U -> ('◒', Green)
            D -> ('◓', Green)
            L -> ('◑', Green)
            R -> ('◐', Green)
        (WorkerOnGoal d) ->
          case d of
            U -> ('◒', Red)
            D -> ('◓', Red)
            L -> ('◑', Red)
            R -> ('◐', Red)
        Wall -> ('▩', Blue)
        Empty -> (' ', White)
        Goal -> ('⁘', Red)
        Box -> ('▩', Yellow)
        BoxOnGoal -> ('▩', Red)

runTest :: IO ()
runTest = do
  gs <- buildGameState []
  let src = gs ^. levelState . worker
  let dst = Point 2 1
  let isAccessible :: Point -> Identity Bool
      isAccessible p = return $ evalState (isEmptyOrGoal <$> getCell p) gs
  let path = runIdentity $ aStarFind src dst isAccessible
  clearScreen
  _ <- moveWorker gs path
  print path
  where
    moveWorker :: GameState -> [Point] -> IO GameState
    moveWorker gs1 [] = return gs1
    moveWorker gs1 (p:ps) = do
      let w = gs1 ^. levelState . worker
      let gs2 =
            case deriveDir w p of
              Just U -> step gs1 A.Up
              Just D -> step gs1 A.Down
              Just L -> step gs1 A.Left
              Just R -> step gs1 A.Right
              _      -> gs1
      moveCursorToOrigin
      render gs2
      threadDelay 100000
      moveWorker gs2 ps

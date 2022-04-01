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
import Control.Monad.State    (MonadState, evalState, runState)
import Data.Char              (isDigit)
import Data.List              (isSuffixOf, stripPrefix)
import Data.Maybe             (fromMaybe, isJust)
import Data.Vector            ((!))
import Sokoban.Level          (Cell(..), Direction(..), LevelCollection(..), Point(..), isBox,
                               isEmptyOrGoal, isWorker, levels)
import Sokoban.Model          (GameState(..), ViewState(..), cells, clicks, destinations, getCell,
                               height, id, initial, levelState, levelState, message, step,
                               viewState, width, worker)
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
            case findDir w p of
              Just U -> step gs1 A.Up
              Just D -> step gs1 A.Down
              Just L -> step gs1 A.Left
              Just R -> step gs1 A.Right
              _      -> gs1
      moveCursorToOrigin
      render gs2
      threadDelay 100000
      moveWorker gs2 ps

findDir :: Point -> Point -> Maybe Direction
findDir (Point i1 j1) (Point i2 j2) =
  case (i2 - i1, j2 - j1) of
    (-1, 0) -> Just U
    (1, 0)  -> Just D
    (0, -1) -> Just L
    (0, 1)  -> Just R
    _       -> Nothing

runConsoleGame :: IO ()
runConsoleGame =
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
      , _viewState = ViewState [] S.empty
      }

gameLoop :: GameState -> IO ()
gameLoop gs = do
  moveCursorToOrigin
  render gs
  key <- getKey
  when (key /= "\ESC") $ do
    let gs1 =
          case key of
            "\ESC[A" -> step gs A.Up
            "\ESC[B" -> step gs A.Down
            "\ESC[C" -> step gs A.Right
            "\ESC[D" -> step gs A.Left
            "u" -> step gs A.Undo
            "i" -> step gs A.Redo
            "r" -> step gs A.Restart
            "\ESC[5~" -> step gs A.PrevLevel
            "\ESC[6~" -> step gs A.NextLevel
            "d" -> step gs A.Debug
            _ ->
              case interpretClick gs key of
                (Just action, gs) -> step gs action
                (Nothing, gs)     -> gs
    -- this is to avoid artifacts from rendering another level or debug
    case key of
      k
        | k `elem` ["\ESC[5~", "\ESC[6~", "d", "r"] -> clearScreen
      _ -> return ()
    gameLoop gs1

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

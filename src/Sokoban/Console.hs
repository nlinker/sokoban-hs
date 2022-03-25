{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Sokoban.Console where

import Prelude hiding (id)

import Control.Exception   (finally)
import Control.Lens        ((^.))
import Control.Monad       (forM_, when)
import Data.Char           (isDigit)
import Data.List           (isSuffixOf, stripPrefix)
import Data.Maybe          (fromMaybe, isJust)
import Data.Vector         ((!))
import Sokoban.Level       (Cell(..), Direction(..), LevelCollection(..), levels)
import Sokoban.Model       (GameState(..), Point(..), cells, height, id, initial, interpretClick,
                            levelState, message, step, width)
import Sokoban.Parser      (parseLevels, splitWith)
import Sokoban.Resources   (microbanCollection)
import System.Console.ANSI (BlinkSpeed(SlowBlink), Color(..), ColorIntensity(..), ConsoleLayer(..),
                            SGR(..), setSGR)
import System.Environment  (getArgs)
import System.IO           (BufferMode(..), hReady, hSetBuffering, hSetEcho, stdin)
import Text.Read           (readMaybe)

import qualified Data.Text     as T
import qualified Data.Text.IO  as T
import qualified Sokoban.Model as A (Action(..))

runConsoleGame :: IO ()
runConsoleGame = do
  args <- getArgs
  levelCollection <-
    if null args
      then return microbanCollection
      else do
        let fileName = head args
        levels0 <- fromMaybe (error $ "Cannot parse file " <> fileName) . parseLevels <$> T.readFile fileName
        let levels = filter (isJust . initial) levels0 -- check invariants for each level as well
        when (null levels) $ error $ "File " <> fileName <> " contains no sokoban levels"
        return $
          LevelCollection
            {_title = T.pack fileName, _description = "", _email = "", _url = "", _copyright = "", _levels = levels}
  -- now we know, that the collection contains at least 1 valid level
  let gameState =
        GameState
          { _collection = levelCollection
          , _index = 0
          , _levelState = fromMaybe (error "Impossible") $ initial $ head (levelCollection ^. levels)
          }
  do hSetBuffering stdin NoBuffering
     hSetEcho stdin False
     clearScreen
     putStrLn "\ESC[?25l" -- hide cursor
     -- enable mouse capturing mode
     putStrLn "\ESC[?1000h"
     putStrLn "\ESC[?1015h"
     putStrLn "\ESC[?1006h"
     gameLoop gameState `finally` do
       putStrLn "\ESC[?25h" -- show cursor
       -- disable mouse capturing mode
       putStrLn "\ESC[?1006l"
       putStrLn "\ESC[?1015l"
       putStrLn "\ESC[?1000l"

gameLoop :: GameState -> IO ()
gameLoop gs = do
  moveCursorToOrigin
  render gs
  key <- getKey
  when (key /= "\ESC") $ do
    let gs1 =
          case key of
            "\ESC[A"  -> step gs A.Up
            "\ESC[B"  -> step gs A.Down
            "\ESC[C"  -> step gs A.Right
            "\ESC[D"  -> step gs A.Left
            "u"       -> step gs A.Undo
            "i"       -> step gs A.Redo
            "r"       -> step gs A.Restart
            "\ESC[5~" -> step gs A.PrevLevel
            "\ESC[6~" -> step gs A.NextLevel
            "d"       -> step gs A.Debug
            _         -> dispatchOther gs key
    -- this is to avoid artifacts from another level rendered
    case key of
      "\ESC[5~" -> clearScreen
      "\ESC[6~" -> clearScreen
      "\"d"     -> clearScreen
      _         -> return ()
    gameLoop gs1
  where
    dispatchOther :: GameState -> String -> GameState
    dispatchOther gs key =
      fromMaybe gs $ do
        click <- extractMouseClick key
        action <- interpretClick gs click
        return $ step gs action
    extractMouseClick :: String -> Maybe (Point, Bool)
    extractMouseClick key = do
      rest <- stripPrefix "\ESC[<0;" key
      -- expected input in the form "\ESC[<0;2;3m" or "\ESC[<0;2;3M"
      let lbmDown = "M" `isSuffixOf` rest
      case readMaybe <$> splitWith isDigit rest :: [Maybe Int] of
        [Just x, Just y] -> Just (Point (y - 1) ((x - 1) `div` 2), lbmDown)
        _                -> Nothing

clearScreen :: IO ()
clearScreen = putStrLn "\ESC[2J"

moveCursorToOrigin :: IO ()
moveCursorToOrigin = putStrLn "\ESC[0;0H"

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
    let (char, color) = renderCell $ (cs ! i) ! j
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
    renderCell :: Cell -> (Char, Color)
    renderCell c =
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

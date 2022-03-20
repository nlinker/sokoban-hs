{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Sokoban.Console where

import Control.Lens        ((^.))
import Control.Monad       (forM_, when)
import Data.Maybe          (fromJust, fromMaybe)
import Sokoban.Level       (Cell(..), Direction(..))
import Sokoban.Model       (GameState, Point(..), cells, getCell, height, initial, step, width , name)
import Sokoban.Parser      (parseLevel, rawLevel)
import System.Console.ANSI (Color(..), ColorIntensity(..), ConsoleLayer(..), SGR(..), setSGR)
import System.IO           (BufferMode(..), hReady, hSetBuffering, hSetEcho, stdin)

import qualified Data.Text           as T
import qualified Sokoban.Model as A (Action(..))

gs0 :: GameState
gs0 = fromJust $ initial =<< parseLevel rawLevel

gs1 :: GameState
gs1 = step gs0 A.Up

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

runConsoleGame :: IO ()
runConsoleGame = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  let gs = fromMaybe (error "Impossible") (initial =<< parseLevel rawLevel)
  render gs
  gameLoop gs
  where
    gameLoop :: GameState -> IO ()
    gameLoop gs = do
      moveCursorUp gs
      render gs
      key <- getKey
      when (key /= "\ESC") $ do
        let gs1 =
              case key of
                "\ESC[A" -> step gs A.Up
                "\ESC[B" -> step gs A.Down
                "\ESC[C" -> step gs A.Right
                "\ESC[D" -> step gs A.Left
                "u"      -> step gs A.Undo
                "r"      -> step gs A.Undo
                _        -> gs
        gameLoop gs1

moveCursorUp :: GameState -> IO ()
moveCursorUp gs = forM_ [0 .. gs ^. height] $ \_ -> putStr "\ESC[A"

render :: GameState -> IO ()
render gs = do
  let cs = gs ^. cells
  let m = gs ^. height
  let n = gs ^. width
  let points = [Point i j | i <- [0 .. m - 1], j <- [0 .. n - 1]]
  putStrLn $ T.unpack $ gs ^. name
  forM_ points $ \p -> do
    let (char, color) = renderCell $ getCell cs p
    let Point _ j = p
    colorStr color $
      if j /= 0
        then " " ++ [char]
        else [char]
    when (j == n - 1) $ putStrLn ""
  where
    colorStr :: Color -> String -> IO ()
    colorStr color str = do
      setSGR [SetColor Foreground Vivid color, SetColor Background Dull Black]
      putStr str
      setSGR []
      putStr ""
    renderCell :: Cell -> (Char, Color)
    renderCell c =
      case c of
        (Worker d) ->
          case d of
            U -> ('▲', Cyan)
            D -> ('▼', Cyan)
            L -> ('◀', Cyan)
            R -> ('▶', Cyan)
        (WorkerOnHole d) ->
          case d of
            U -> ('▲', Blue)
            D -> ('▼', Blue)
            L -> ('◀', Blue)
            R -> ('▶', Blue)
        Wall -> ('■', Yellow)
        Empty -> (' ', White)
        Hole -> ('⨯', Blue)
        Box -> ('☐', Red)
        BoxOnHole -> ('☒', Red)
{-
variants:
U '▲' '△'
D '▼' '▽'
L '◀' '◁'
R '▶' '▷'
-}

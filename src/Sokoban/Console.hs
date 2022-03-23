{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Sokoban.Console where

import Control.Lens        ((^.))
import Control.Monad       (forM_, when)
import Data.Maybe          (fromMaybe, isJust)
import Data.Vector         ((!))
import Sokoban.Level       (Cell(..), Direction(..), LevelCollection(..), levels)
import Sokoban.Model       (GameState(..), Point(..), cells, height, initial,
                            isComplete, name, step, width, levelState, message)
import Sokoban.Parser      (parseLevels)
import Sokoban.Resources   (microbanCollection)
import System.Console.ANSI (Color(..), ColorIntensity(..), ConsoleLayer(..), SGR(..), setSGR)
import System.Environment  (getArgs)
import System.IO           (BufferMode(..), hReady, hSetBuffering, hSetEcho, stdin)

import qualified Data.Text     as T
import qualified Data.Text.IO  as T
import qualified Sokoban.Model as A (Action(..))

-- tracking mouse clicks inside terminal window
-- https://stackoverflow.com/a/5970472/5066426
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

  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  gameLoop gameState

--startGameLoop :: GameState -> IO ()
--startGameLoop = do


gameLoop :: GameState -> IO ()
gameLoop gs = do
  moveCursorUp gs
  render gs
  -- TODO this is weird
  when (gs ^. levelState . isComplete) $ error "Level complete"
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
            _         -> gs
    gameLoop gs1

moveCursorUp :: GameState -> IO ()
moveCursorUp _gs = do
  putStr "\ESC[2J"
  putStr "\ESC[0;0H"
  --  let hs = gs ^. holes
  --  let bs = gs ^. boxes
  --  let isc = gs ^. isComplete
  --  putStrLn $ "hs = " <> show hs <> " bs = " <> show bs <> " isComplete = " <> show isc
  --  forM_ [0 .. gs ^. height + 21] $ \_ -> putStr "\ESC[A"

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
    colorStr color $
      if j /= 0
        then " " ++ [char]
        else [char]
    when (j == n - 1) $ putStrLn ""
  T.putStrLn $ "Level: " <> ls ^. name
  T.putStrLn $ ls ^. message
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
            U -> ('▲', Green)
            D -> ('▼', Green)
            L -> ('◀', Green)
            R -> ('▶', Green)
        (WorkerOnHole d) ->
          case d of
            U -> ('▲', Yellow)
            D -> ('▼', Yellow)
            L -> ('◀', Yellow)
            R -> ('▶', Yellow)
        Wall -> ('■', Blue)
        Empty -> (' ', White)
        Hole -> ('⨯', Yellow)
        Box -> ('☐', Red)
        BoxOnHole -> ('☒', Red)
{-
variants:
U '▲' '△'
D '▼' '▽'
L '◀' '◁'
R '▶' '▷'
-}

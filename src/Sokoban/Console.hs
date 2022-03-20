{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Sokoban.Console where

import Control.Monad       (forM_, when)
import Sokoban.Model       (Cell(..), Direction(..), GameState(..), Point(..))
import System.Console.ANSI (Color(..), ColorIntensity(..), ConsoleLayer(..), SGR(..), setSGR)
import System.IO           (BufferMode(..), hReady, hSetBuffering, hSetEcho, stdin)

import qualified Data.HashMap.Strict as M

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
  key <- getKey
  when (key /= "\ESC") $ do
    case key of
      "\ESC[A" -> putStr "↑"
      "\ESC[B" -> putStr "↓"
      "\ESC[C" -> putStr "→"
      "\ESC[D" -> putStr "←"
      "\n"     -> putStr "⎆"
      "\DEL"   -> putStr "⎋"
      _        -> return ()
    runConsoleGame

render :: GameState -> IO ()
render gs = do
  let cs = cells (gs :: GameState)
  let m = height (gs :: GameState)
  let n = width (gs :: GameState)
  let points = [Point i j | i <- [0 .. m - 1], j <- [0 .. n - 1]]
  forM_ points $ \p -> do
    let (char, color) =
          case M.lookup p cs of
            Just c  -> renderCell c
            Nothing -> error "Should be impossible"
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
            U -> ('▲', White)
            D -> ('▼', White)
            L -> ('◀', White)
            R -> ('▶', White)
        (WorkerOnHole d) ->
          case d of
            U -> ('▲', Cyan)
            D -> ('▼', Cyan)
            L -> ('◀', Cyan)
            R -> ('▶', Cyan)
        Wall -> ('▮', Yellow)
        Empty -> (' ', White)
        Hole -> ('⨯', Blue)
        Box -> ('⊡', Red)
        BoxOnHole -> ('⊠', Magenta)

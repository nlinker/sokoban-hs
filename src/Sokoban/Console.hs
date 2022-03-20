{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Sokoban.Console where

import Control.Monad       (forM_, when)
import Data.Maybe          (fromJust)
import Data.Vector         (Vector, (!))
import Sokoban.Model       (Action(..), Cell(..), Direction(..), GameState(..), Point(..), initial,
                            step)
import Sokoban.Parser      (parseLevel, rawLevel)
import System.Console.ANSI (Color(..), ColorIntensity(..), ConsoleLayer(..), SGR(..), setSGR)
import System.IO           (BufferMode(..), hReady, hSetBuffering, hSetEcho, stdin)

import qualified Data.HashMap.Strict as M
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV

gs0 :: GameState
gs0 = fromJust $ initial =<< parseLevel rawLevel

gs1 :: GameState
gs1 = step gs0 Up

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

x :: Vector (Vector Cell)
x = V.fromList [V.fromList [Empty, Empty], V.fromList [Wall, Wall]]

y = put x 0 0 Wall

z = put x 0 1 Wall

put :: Vector (Vector Cell) -> Int -> Int -> Cell -> Vector (Vector Cell)
put v0 i j cell =
  let row = V.modify (\v -> MV.write v j cell) (v0 ! i)
   in V.modify (\vv -> MV.write vv i row) v0

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

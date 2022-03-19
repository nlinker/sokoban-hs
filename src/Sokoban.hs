{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Sokoban where

import Control.Monad.Identity (runIdentity)
import Data.Char              (isSpace)
import Data.Hashable          (Hashable)
import Data.List              (find, partition)
import Data.List.Extra        (dropEnd)
import Data.Maybe             (fromJust, fromMaybe, isJust, listToMaybe)
import Data.Tuple.Extra       (fst3)
import Helper                 (str)
import System.Console.ANSI    (Color(..), ColorIntensity(..), ConsoleLayer(..), SGR(..), setSGR)

import qualified Data.ByteString       as B (ByteString)
import qualified Data.ByteString.Char8 as B (all, lines, unpack)
import qualified Data.HashMap.Strict   as M
import qualified Data.Text             as T
import           GHC.Generics          (Generic)

main :: IO ()
main = undefined

data Direction
  = U
  | D
  | L
  | R
  deriving (Eq, Show)

data Point =
  Point Int Int
  deriving (Eq, Show, Generic, Hashable)

data Cell
  = Worker Direction
  | Hole
  | Box
  | Wall
  | Empty
  | BoxOnHole
  | WorkerOnHole Direction
  deriving (Eq, Show)

data Level =
  Level
    { cells  :: [[Cell]]
    , height :: Int
    , width  :: Int
    , name   :: T.Text
    }
  deriving (Eq, Show)

data GameState =
  GameState
    { cells      :: M.HashMap Point Cell
    , height     :: Int
    , width      :: Int
    , name       :: T.Text
    , worker     :: Point
    , workerDrc  :: Direction
    , blocks     :: [Point]
    , isComplete :: Bool
    }

colorStrLn :: ColorIntensity -> Color -> ColorIntensity -> Color -> String -> IO ()
colorStrLn fgi fg bgi bg str = do
  setSGR [SetColor Foreground fgi fg, SetColor Background bgi bg]
  putStr str
  setSGR []
  putStrLn ""

testColor :: IO ()
testColor = do
  colorStrLn Vivid White Vivid Red "This is red on white."
  colorStrLn Vivid White Dull Blue "This is white on blue."
  colorStrLn Vivid Green Dull Black "This is green on black."
  colorStrLn Vivid Yellow Dull Black "This is yellow on black."
  colorStrLn Dull Black Vivid Blue "This is black on light blue."

initial :: Level -> GameState
initial level = gameState
  where
    worker = undefined
    gameState =
      GameState
        { cells = cellsConvert --  $ cells (level :: Level)
        , height = height (level :: Level)
        , width = width (level :: Level)
        , name = name (level :: Level)
        , worker = worker
        , workerDrc = U
        , blocks = []
        , isComplete = False
        }

cellsConvert :: M.HashMap Point Cell
cellsConvert =
  runIdentity $ do
    let level = fromJust $ parseLevel rawLevel
    let h = height (level :: Level)
    let w = width (level :: Level)
    let cs = concat $ cells (level :: Level)
    let points = [Point (i - 1) (j - 1) | i <- [1 .. h], j <- [1 .. w]]
    return $ M.fromList $ zip points cs
  where
    _findCoords = undefined

-- We use screen (not Decartes) coordinates (i, j).
-- The origin is in the upper left corner.
delta :: Direction -> Point
delta U = Point 0 (-1)
delta D = Point 0 1
delta L = Point (-1) 0
delta R = Point 1 0

parseCell :: Char -> Maybe Cell
parseCell c =
  case c of
    '@' -> Just (Worker U)
    '+' -> Just (WorkerOnHole U)
    '#' -> Just Wall
    ' ' -> Just Empty
    '.' -> Just Hole
    '$' -> Just Box
    '*' -> Just BoxOnHole
    _   -> Nothing

-- break string into lines
parseLevel :: B.ByteString -> Maybe Level
parseLevel raw = do
  let xs = map B.unpack $ filter (not . B.all isSpace) $ B.lines raw
  -- separate the lines prefixed with ";"
  let (ds, ys) = partition (isJust . find (== ';')) xs
  let name = buildDescription ds
  let (field, m, n) = normalize ys
  -- let level' = sequenceA $ map (sequenceA . map parseCell) field
  let cells' = traverse (traverse parseCell) field
  case cells' of
    Just cells -> Just Level {cells = cells, height = m, width = n, name = name}
    Nothing    -> Nothing

buildDescription :: [String] -> T.Text
buildDescription xs = T.pack $ concat pieces
  where
    convert x = tail $ dropWhile (/= ';') x
    pieces = map convert xs

normalize :: [String] -> ([String], Int, Int)
normalize xs = (field, rowCount, colCount)
  where
    nPrefix = length $ foldr1 (commonPrefix isSpace) xs
    lengths = map length xs
    maxLen = maximum lengths
    nAppends = map (maxLen -) lengths
    ys = map (\(s, n) -> s ++ replicate n ' ') $ zip xs nAppends
    nSuffix = length $ foldr1 (commonSuffix isSpace) ys
    trimBoth s = dropEnd nSuffix $ drop nPrefix s
    field = deinterlace $ map trimBoth ys
    rowCount = length field
    colCount = length $ fromMaybe [] $ listToMaybe field

deinterlace :: [String] -> [String]
deinterlace xs =
  let zs = map (zip [0 ..]) xs
   in if all isSpace $ concatMap (map snd . filter odds) zs
        -- the field is space-interleaved
        then map (map snd . filter evens) zs
        -- the field is compact
        else map (map snd) zs

odds :: (Integer, b) -> Bool
odds (i, _) = i `mod` 2 == 1

evens :: (Integer, b) -> Bool
evens (i, _) = i `mod` 2 == 0

commonPrefix :: Eq a => (a -> Bool) -> [a] -> [a] -> [a]
commonPrefix _ _ [] = []
commonPrefix _ [] _ = []
commonPrefix p (x:xs) (y:ys) =
  if x == y && p x
    then x : commonPrefix p xs ys
    else []

commonSuffix :: Eq a => (a -> Bool) -> [a] -> [a] -> [a]
commonSuffix p xs ys = commonPrefix p (reverse xs) (reverse ys)

rawLevel :: B.ByteString
rawLevel =
  [str|
      # # # # #
    # #   .   #
    #   .   $ #
  # # $ # . . #
  # @ $   *   #
  #   $       #
  # # #       #
      # # # # #
  ; The maze
; number 1
|]

rawLevelCompressed :: B.ByteString
rawLevelCompressed =
  [str|
    #####
   ## . #
   # . $#
  ##$#..#
  #@$ * #
  # $   #
  ###   #
    #####
  ; The maze
  ; number 2
|]

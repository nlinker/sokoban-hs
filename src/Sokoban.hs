{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Sokoban where

import Data.Char             (isSpace)
import Data.Functor.Identity (runIdentity)
import Data.List             (find, partition)
import Data.List.Extra       (dropEnd)
import Data.Maybe            (isJust)
import Helper                (str)

import qualified Data.ByteString       as B (ByteString)
import qualified Data.ByteString.Char8 as B (all, lines, unpack)
import qualified Data.HashMap.Strict   as M
import qualified Data.Text             as T

main :: IO ()
main = undefined

data Direction
  = U
  | D
  | L
  | R
  deriving (Eq, Show)

data Point =
  Point
    { x :: Integer
    , y :: Integer
    }
  deriving (Eq, Show)

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
    { cells        :: M.HashMap Point Cell
    , charPosition :: Point
    , name         :: T.Text
    }
  deriving (Eq, Show)

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
  let desc = buildDescription ds
  let maze = normalize ys
  -- let xs    = map (B.takeWhile isSpace) chars
  -- let ijs   = [[(i, j) | i <- [0 ..]] | j <- [0 ..]]
  -- let elems = concat $ zipWith zip ijs chars
  -- let cells :: [((Integer, Integer), Maybe Cell)]
  --     cells = map (second parseCell) elems
  Nothing

buildDescription :: [String] -> String
buildDescription xs = concat pieces
  where
    convert x = tail $ dropWhile (/= ';') x
    pieces = map convert xs

test :: [String]
test =
  runIdentity $ do
    let xs = map B.unpack $ filter (not . B.all isSpace) $ B.lines rawLevel
  -- separate the lines prefixed with ";"
    let (ds, ys) = partition (isJust . find (== ';')) xs
    let desc = buildDescription ds
    let maze = normalize ys
    return maze

normalize :: [String] -> [String]
normalize xs = deinterlace $ map trimBoth ys
  where
    nPrefix = length $ foldr1 (commonPrefix isSpace) xs
    lengths = map length xs
    maxLen = maximum lengths
    nAppends = map (maxLen -) lengths
    ys = map (\(s, n) -> s ++ replicate n ' ') $ zip xs nAppends
    nSuffix = length $ foldr1 (commonSuffix isSpace) ys
    trimBoth s = dropEnd nSuffix $ drop nPrefix s

deinterlace :: [String] -> [String]
deinterlace xs =
  let zs = map (zip [0 ..]) xs
   in if all isSpace $ concatMap (map snd . filter odds) zs
        -- the maze is space-interleaved
        then map (map snd . filter evens) zs
        -- the maze is compact
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

level :: Maybe Level
level = parseLevel rawLevel

rawLevel :: B.ByteString
rawLevel =
  [str|
      # # # # #
    # #   .   #
    #   .   $ #
  # # $ # . . #
  # @ $   *   # #
  #   $         #
  # # #       # #
      # # # # #
  ; The maze
; number 1
|]

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Sokoban.Parser where

import Data.Char       (isSpace)
import Data.List       (find, partition)
import Data.List.Extra (dropEnd)
import Data.Maybe      (fromMaybe, isJust, listToMaybe)
import Helper          (str)
import Sokoban.Level   (Cell(..), Direction(..), Level(..))

import qualified Data.Text as T

parseLevels :: T.Text -> Maybe [Level]
parseLevels raw = sequenceA $ parseLevel <$> (splitWith (not . T.all isSpace) . T.lines $ raw)

parseLevel :: [T.Text] -> Maybe Level
parseLevel rawLines = do
  let xs = map T.unpack $ filter (not . T.all isSpace) rawLines
  -- separate the lines prefixed with ";"
  let (ds, ys) = partition (isJust . find (== ';')) xs
  let name = buildDescription ds
  let (field, h, w) = normalize ys
  -- let level' = sequenceA $ map (sequenceA . map parseCell) field
  let cells' = traverse (traverse parseCell) field
  case cells' of
    Just cells -> Just Level {_cells = cells, _height = h, _width = w, _id = name}
    Nothing    -> Nothing

parseCell :: Char -> Maybe Cell
parseCell c =
  case c of
    '@' -> Just (Worker D)
    '+' -> Just (WorkerOnGoal D)
    '#' -> Just Wall
    ' ' -> Just Empty
    '.' -> Just Goal
    '$' -> Just Box
    '*' -> Just BoxOnGoal
    _   -> Nothing

buildDescription :: [String] -> T.Text
buildDescription xs = T.pack $ dropWhile (== ' ') $ concat pieces
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

-- taken from https://stackoverflow.com/a/19927596/5066426
splitWith :: (t -> Bool) -> [t] -> [[t]]
splitWith pred (x:xs)
  | pred x =
    let (first, rest) = span pred (x : xs)
     in first : splitWith pred rest
  | otherwise = splitWith pred xs
splitWith _pred [] = []

rawLevel :: String
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

rawDefaultCollection :: String
rawDefaultCollection =
  [str|
  # # # # # #
  # @ $   . #
  # # # # # #
  ; Simplest maze ever
|]


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
import Sokoban.Model   (Cell(..), Direction(..), Level(..))

import qualified Data.ByteString       as B (ByteString)
import qualified Data.ByteString.Char8 as B (all, lines, unpack)
import qualified Data.Text             as T

parseCell :: Char -> Maybe Cell
parseCell c =
  case c of
    '@' -> Just (Worker D)
    '+' -> Just (WorkerOnHole D)
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
  let (field, h, w) = normalize ys
  -- let level' = sequenceA $ map (sequenceA . map parseCell) field
  let cells' = traverse (traverse parseCell) field
  case cells' of
    Just cells -> Just Level {cells = cells, height = h, width = w, name = name}
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

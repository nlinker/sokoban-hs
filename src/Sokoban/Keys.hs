module Sokoban.Keys where

import Control.Concurrent.STM (TChan, atomically, writeTChan)
import Control.Monad (forM_, join)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Char (isDigit)
import Data.List (isSuffixOf, stripPrefix)
import Data.Maybe (fromMaybe)
import Sokoban.Level (Direction (..), Point (..))
import Sokoban.Parser (splitWith)
import System.IO (hReady, stdin)
import Text.Read (readMaybe)

data Key
  = Arrow Direction
  | PageUp
  | PageDown
  | Escape
  | Letter Char
  | MouseClick (Point, Bool)
  deriving (Eq, Show)

letters :: String
letters = ['[', ']'] <> ['a' .. 'z']

keyLoop :: TChan msg -> (Key -> Maybe msg) -> IO ()
keyLoop chan toMsg = do
  key <- getKey
  let k' =
        case key of
          "\ESC[A" -> Just (Arrow U)
          "\ESC[B" -> Just (Arrow D)
          "\ESC[D" -> Just (Arrow L)
          "\ESC[C" -> Just (Arrow R)
          "\ESC[5~" -> Just PageUp
          "\ESC[6~" -> Just PageDown
          "\ESC" -> Just Escape
          [l]
            | l `elem` letters -> Just (Letter l)
          _ ->
            case extractMouseClick key of
              Just (point, lbmDown) -> Just (MouseClick (point, lbmDown))
              Nothing -> Nothing
  forM_ (k' >>= toMsg) $ atomically . writeTChan chan
  --  case toMsg =<< k' of
  --    Just msg -> atomically $ writeTChan chan msg
  --    Nothing -> pure ()
  keyLoop chan toMsg

getKey :: IO String
getKey = reverse <$> getKey' ""
  where
    getKey' chars = do
      char <- getChar
      more <- hReady stdin
      ( if more
          then getKey'
          else return
        )
        (char : chars)

extractMouseClick :: String -> Maybe (Point, Bool)
extractMouseClick key = do
  rest <- stripPrefix "\ESC[<0;" key
  -- expected input in the form "\ESC[<0;2;3M" or "\ESC[<0;2;3m" ("m" is button up)
  let lbmDown = "M" `isSuffixOf` rest
  case readMaybe <$> splitWith isDigit rest :: [Maybe Int] of
    [Just x, Just y] -> Just (Point (y - 2) ((x - 3) `div` 2), lbmDown)
    _ -> Nothing

{-# LANGUAGE BinaryLiterals        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveAnyClass        #-}

module Sokoban.Console where

import Prelude hiding (id)

import Control.Concurrent         (threadDelay)
import Control.Exception          (finally)
import Control.Lens               (use, (&), (.=), (.~), (^.), _1)
import Control.Lens.TH            (makePrisms)
import Control.Monad              (filterM, forM_, when)
import Control.Monad.State        (MonadState, State, evalState, execState, get, runState)
import Data.Char                  (isDigit)
import Data.Hashable              (Hashable)
import Data.List                  (isSuffixOf, stripPrefix)
import Data.Maybe                 (fromMaybe, isJust)
import Data.Vector                ((!))
import GHC.Generics               (Generic)
import Sokoban.Level              (Cell(..), Direction(..), LevelCollection(..), Point(..),
                                   deriveDir, isBox, isEmptyOrGoal, isWorker, levels, movePoint,
                                   opposite)
import Sokoban.Model              (GameState(..), UndoItem(..), ViewState(..), animateForward,
                                   animateRequired, cells, clicks, destinations, direction,
                                   doClearScreen, doMove, getCell, height, id, initial, levelState,
                                   levelState, message, step, undoIndex, undoMove, undoStack,
                                   updateCell, viewState, width, worker, _UndoItem)
import Sokoban.Parser             (parseLevels, splitWith)
import Sokoban.Resources          (yoshiroAutoCollection)
import Sokoban.Solver             (AStarSolver(..), aStarFind)
import System.Console.ANSI        (BlinkSpeed(SlowBlink), Color(..), ColorIntensity(..),
                                   ConsoleLayer(..), SGR(..), setSGR)
import System.Environment         (getArgs)
import System.IO                  (BufferMode(..), hReady, hSetBuffering, hSetEcho, stdin)
import Text.InterpolatedString.QM (qm)
import Text.Read                  (readMaybe)

import qualified Data.HashSet  as S
import qualified Data.Text     as T
import qualified Data.Text.IO  as T
import qualified Sokoban.Model as A (Action(..))

-- directed point, we can use this for boxes
data PD =
  PD Point Direction [Direction]
  deriving (Eq, Generic, Hashable)

makePrisms ''PD

instance Show PD where
  show (PD (Point i j) d dirs) = [qm|({i} {j} {d} {dirs})|]

instance Ord PD where
  compare (PD p1 d1 ds1) (PD p2 d2 ds2) = compare (p1, d1, ds1) (p2, d2, ds2)

whenWith :: Monad m => a -> (a -> Bool) -> m a -> m a
whenWith a p runA =
  if p a
    then runA
    else return a

-- | run console game
run :: IO ()
run =
  do args <- getArgs
     gameState <- buildGameState args
     do setupScreen
        gameLoop gameState
     `finally` destroyScreen
  where
    setupScreen = do
      hSetBuffering stdin NoBuffering
      hSetEcho stdin False
      clearScreen
        -- hide cursor
      putStrLn "\ESC[?25l"
       -- enable mouse capturing mode
      putStrLn "\ESC[?1000h"
      putStrLn "\ESC[?1015h"
      putStrLn "\ESC[?1006h"
    destroyScreen = do
      putStrLn "\ESC[?25h" -- show cursor
       -- disable mouse capturing mode
      putStrLn "\ESC[?1006l"
      putStrLn "\ESC[?1015l"
      putStrLn "\ESC[?1000l"

buildGameState :: [String] -> IO GameState
buildGameState args = do
  levelCollection <-
    if null args
      then return yoshiroAutoCollection -- default
      else do
        let fileName = head args
        levels0 <- fromMaybe (error $ "Cannot parse file " <> fileName) . parseLevels <$> T.readFile fileName
        let levels = filter (isJust . initial) levels0 -- check invariants for each level as well
        when (null levels) $ error $ "File " <> fileName <> " contains no sokoban levels"
        return $
          LevelCollection
            {_title = T.pack fileName, _description = "", _email = "", _url = "", _copyright = "", _levels = levels}
  -- now we know, that the collection contains at least 1 valid level
  return $
    GameState
      { _collection = levelCollection
      , _index = 0
      , _levelState = fromMaybe (error "Impossible") $ initial $ head (levelCollection ^. levels)
      , _viewState = ViewState False [] S.empty False False
      }

gameLoop :: GameState -> IO ()
gameLoop gs0
  -- if we need to draw multiple
 = do
  moveCursorToOrigin
  render gs0
  key <- getKey
  when (key /= "\ESC") $ do
    let gs1 =
          case key of
            "\ESC[A" -> step gs0 A.Up
            "\ESC[B" -> step gs0 A.Down
            "\ESC[C" -> step gs0 A.Right
            "\ESC[D" -> step gs0 A.Left
            "\ESC[5~" -> step gs0 A.PrevLevel
            "\ESC[6~" -> step gs0 A.NextLevel
            "u" -> step gs0 A.Undo
            "i" -> step gs0 A.Redo
            "r" -> step gs0 A.Restart
            "d" -> step gs0 A.Debug
            _
              -- A.MoveWorker dstPoint and A.MoveBoxes srcBoxes dstBoxes
             ->
              case interpretClick gs0 key of
                (Just action, gs) -> step gs action
                (Nothing, gs)     -> gs
    -- perform animation if needed
    gs2 <-
      whenWith gs1 (^. (viewState . animateRequired)) $ do
        animate gs0 gs1
        return $ gs1 & viewState . animateRequired .~ False & viewState . animateForward .~ False
    -- clear screen if needed
    gs3 <-
      whenWith gs2 (^. (viewState . doClearScreen)) $ do
        clearScreen
        return $ gs2 & viewState . doClearScreen .~ False
    gameLoop gs3

animate :: GameState -> GameState -> IO ()
animate gsFrom gsTo = do
  let undos = gsTo ^. levelState . undoStack
  let uidx = gsTo ^. levelState . undoIndex
  if gsTo ^. viewState . animateForward
    then do
      let dirs = map (^. direction) $ (undos !! uidx) ^. _UndoItem
      animateDo gsFrom dirs
    else do
      let diffs = reverse $ (undos !! (uidx - 1)) ^. _UndoItem
      animateUndo gsFrom diffs
  where
    animateDo _ [] = return ()
    animateDo gs (dir:dirs) = do
      let gs2 = execState (doMove dir) gs
      moveCursorToOrigin
      render gs2
      threadDelay (50 * 1000)
      animateDo gs2 dirs
    animateUndo _ [] = return ()
    animateUndo gs (diff:diffs) = do
      let gs2 = execState (undoMove diff) gs
      moveCursorToOrigin
      render gs2
      threadDelay (50 * 1000)
      animateUndo gs2 diffs

interpretClick :: GameState -> String -> (Maybe A.Action, GameState)
interpretClick gs key = runState runInterpretClick gs
  where
    runInterpretClick :: MonadState GameState m => m (Maybe A.Action)
    runInterpretClick =
      case extractMouseClick key of
        Nothing -> return Nothing
        Just (_, True) -> return Nothing
        Just (click, False) -> do
          clickz <- (click :) <$> use (viewState . clicks)
          cellz <- mapM getCell clickz
          case (clickz, cellz) of
            ([], []) -> return Nothing
            ([p0], [c0])
              | isEmptyOrGoal c0 -> do
                viewState . clicks .= []
                return $ Just $ A.MoveWorker p0
              | isWorker c0 -> do
                viewState . clicks .= [p0]
                return Nothing
              | isBox c0 -> do
                viewState . clicks .= [p0]
                return Nothing
              | otherwise -> do
                viewState . clicks .= []
                return Nothing
            ([p1, p0], [c1, c0])
              | isWorker c0 && isDestination c1 -> do
                viewState . clicks .= []
                return $ Just $ A.MoveWorker p1
              | isBox c0 && isDestination c1 -> do
                viewState . clicks .= []
                return $ Just $ A.MoveBoxes [p0] [p1]
              | isBox c0 && isBox c1 -> do
                viewState . clicks .= [p1, p0]
                return Nothing
              | otherwise -> do
                viewState . clicks .= [] -- reset tracking clicks
                return Nothing
            ([p2, p1, p0], [c2, c1, c0])
              | isBox c0 && isBox c1 && isDestination c2 -> do
                viewState . clicks .= [p2, p1, p0]
                return Nothing
              | otherwise -> do
                viewState . clicks .= []
                return Nothing
            ([p3, p2, p1, p0], [c3, c2, c1, c0])
              | isBox c0 && isBox c1 && isDestination c2 && isDestination c3 -> do
                viewState . clicks .= []
                return $ Just $ A.MoveBoxes [p0, p1] [p2, p3]
            ([p3, p2, p1, p0], [c3, c2, c1, c0])
              | isBox c0 && isBox c1 && isDestination c2 && isBox c3 -> do
                viewState . clicks .= []
                return $ Just $ A.MoveBoxes [p0, p1] [p2, p3]
              | otherwise -> do
                viewState . clicks .= []
                return Nothing
            _ -> do
              viewState . clicks .= [] -- reset tracking clicks
              viewState . destinations .= S.empty
              return Nothing

isDestination :: Cell -> Bool
isDestination c =
  case c of
    Worker _       -> True
    WorkerOnGoal _ -> True
    Goal           -> True
    Empty          -> True
    _              -> False

extractMouseClick :: String -> Maybe (Point, Bool)
extractMouseClick key = do
  rest <- stripPrefix "\ESC[<0;" key
  -- expected input in the form "\ESC[<0;2;3M" or "\ESC[<0;2;3m" ("m" is button up)
  let lbmDown = "M" `isSuffixOf` rest
  case readMaybe <$> splitWith isDigit rest :: [Maybe Int] of
    [Just x, Just y] -> Just (Point (y - 2) ((x - 1) `div` 2), lbmDown)
    _                -> Nothing

showInMessage :: Show a => GameState -> a -> GameState
showInMessage gs x =
  let nm = T.length $ gs ^. levelState . message
      msg1 = "action = " <> show x
   in gs & levelState . message .~ T.pack (msg1 <> replicate (nm - length msg1) ' ')

clearScreen :: IO ()
clearScreen = putStrLn "\ESC[2J"

moveCursorToOrigin :: IO ()
moveCursorToOrigin = putStrLn "\ESC[1;1H"

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
  let vs = gs ^. viewState
  let cs = ls ^. cells
  let m = ls ^. height
  let n = ls ^. width
  let points = [Point i j | i <- [0 .. m - 1], j <- [0 .. n - 1]]
  forM_ points $ \p -> do
    let Point i j = p
    let (char, color) = getCellSkin $ (cs ! i) ! j
    let suffix =
          if j /= 0
            then " " ++ [char]
            else [char]
    if p `elem` (vs ^. clicks)
      then colorStr color True suffix
      else colorStr color False suffix
    when (j == n - 1) $ putStrLn ""
  T.putStrLn $ "Level: " <> ls ^. id
  T.putStrLn $ ls ^. message
  where
    colorStr :: Color -> Bool -> String -> IO ()
    colorStr color selected str = do
      setSGR $
        if selected
          then [SetColor Foreground Vivid color, SetColor Background Dull Black, SetBlinkSpeed SlowBlink]
          else [SetColor Foreground Vivid color, SetColor Background Dull Black]
      putStr str
      setSGR []
      putStr ""
    getCellSkin :: Cell -> (Char, Color)
    getCellSkin c =
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

-------------------------------------------------------------------------------
runTest :: IO ()
runTest = do
  gsFirst <- buildGameState []
  let gsSecond = step gsFirst A.NextLevel
  render gsSecond
  let gsThird = eraseBox gsSecond (Point 4 2)
  let gs = gsThird
  let w = gs ^. levelState . worker
  let src = PD (Point 4 2) D [] -- box directed to the right
  let dst = PD (Point 1 1) U []
  
  let neighbors :: PD -> State GameState [PD]
      neighbors (PD p0 d0 _ds0) = do
        let moveSolver = buildMoveSolver gs p0 :: AStarSolver (State GameState) Point
        let isAccessible p = isEmptyOrGoal <$> getCell p
        let stopCond dst = return . (== dst)
        let tryBuildPath src dst = do
              aStarFind moveSolver src dst (return . (== dst))
              undefined
        let isPathAccessible src dst = do
              accessible <- isAccessible dst
              if accessible
                then not . null <$> aStarFind moveSolver src dst (return . (== dst))
                else return False
        cont <- filterM (isAccessible . (^. _PD . _1)) [PD (movePoint p0 d0) d0 [d0]]
        let src = movePoint p0 (opposite d0)
        let dsts1 = map (\d -> PD p0 d []) $ filter (/= d0) [U, D, L, R]
        (cont <>) <$> filterM (\(PD _ d _) -> isPathAccessible src (movePoint p0 (opposite d))) dsts1
  let heuristic (PD (Point i1 j1) _ _) (PD (Point i2 j2) _ _) = return $ abs (i1 - i2) + abs (j1 - j2)
  let distance np p0 = return $ fromEnum (np /= p0)
  let pushSolver = AStarSolver {neighbors = neighbors, distance = distance, heuristic = heuristic}
  let pushPath = evalState (aStarFind pushSolver src dst (return . (== dst))) gs
  let dirs = dPathToDirections gs pushPath
  let gsFourth =
        flip execState gsSecond $ do
          diffs' <- sequenceA <$> mapM doMove dirs
          case diffs' of
            Nothing -> return ()
            Just [] -> return ()
            Just diffs -> do
              ls <- use levelState
              let uidx = ls ^. undoIndex
              levelState . undoStack .= UndoItem diffs : drop uidx (ls ^. undoStack)
              levelState . undoIndex .= 0
              viewState . animateRequired .= True
              viewState . animateForward .= True
  clearScreen
  animate gsSecond gsFourth
  where
    eraseBox :: GameState -> Point -> GameState
    eraseBox gs p =
      flip execState gs $ do
        c <- getCell p
        case c of
          Box       -> updateCell p Empty
          BoxOnGoal -> updateCell p Goal
          _         -> return ()

--     & viewState . animateRequired .~
dPathToDirections :: GameState -> [PD] -> [Direction]
dPathToDirections _gs [] = []
dPathToDirections gs pds =
  let PD p d ds = head pds
   in flip evalState gs $ do
        let w = gs ^. levelState . worker
        prefix <- astar p w (movePoint p $ opposite d)
        conv <- convert pds []
        return $ prefix <> reverse conv
  where
    astar :: MonadState GameState m => Point -> Point -> Point -> m [Direction]
    astar box s t = do
      gs <- get
      let solver = buildMoveSolver gs box
      pathToDirections <$> aStarFind solver s t (return . (== t))
    --    convert ps acc
    --      | trace [qm| \nps={ps}\nacc={acc}|] False = undefined
    convert :: MonadState GameState m => [PD] -> [Direction] -> m [Direction]
    convert [] _acc = return []
    convert [PD {}] acc = return acc
    convert (PD _ d1 _ds1:PD p d2 ds2:ps) acc
      | d1 == d2 = convert (PD p d2 ds2 : ps) (d2 : acc)
    convert (PD p1 d1 _ds1:PD p2 d2 ds2:ps) acc -- p1 == p2
     = do
      let s = movePoint p1 $ opposite d1
      let t = movePoint p2 $ opposite d2
      path <- astar p2 s t
      convert (PD p2 d2 ds2 : ps) (reverse path <> acc)

pathToDirections :: [Point] -> [Direction]
pathToDirections ps = reverse $ convert ps []
  where
    convert [] _acc = []
    convert [_] acc = acc
    convert (p1:p2:ps) acc =
      case deriveDir p1 p2 of
        Nothing -> acc
        Just d  -> convert (p2 : ps) (d : acc)

buildMoveSolver :: Monad m => GameState -> Point -> AStarSolver m Point
buildMoveSolver gs box = AStarSolver {neighbors = neighbors, distance = distance, heuristic = heuristic}
  where
    neighbors p0 =
      return $ do
        let isAccessible p = (p /= box) && isEmptyOrGoal (evalState (getCell p) gs)
        let neighs = map (movePoint p0) [U, D, L, R]
        filter isAccessible neighs
    distance np p0 = return $ fromEnum (np /= p0)
    heuristic (Point i1 j1) (Point i2 j2) = return $ abs (i1 - i2) + abs (j1 - j2)

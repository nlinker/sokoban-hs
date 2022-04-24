{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sokoban.ModelSpec where

import Control.Arrow          (second)
import Control.Lens           ((&), (.~), (^.))
import Control.Monad.Identity (runIdentity)
import Control.Monad.State    (evalState)
import Data.Maybe             (fromJust)
import Sokoban.Console        (interpretClick, render)
import Sokoban.Level          (Cell(..), Direction(..), Point(..), fromCell, isEmptyOrGoal, levels,
                               movePoint, toCell)
import Sokoban.Model          (GameState(..), ViewState(..), clicks, getCell, initialLevelState, levelState,
                               viewState, worker, pathToDirections, width, height)
import Sokoban.Resources      (yoshiroAutoCollection)
import Sokoban.Solver         (AStarSolver(..), aStarFind, breadFirstFind)

import qualified Data.HashSet  as S
import qualified Sokoban.Model as A (Action(..))

import Test.Hspec

gs :: GameState
gs =
  GameState
    { _collection = yoshiroAutoCollection
    , _index = 0
    , _levelState = fromJust $ initialLevelState $ head $ yoshiroAutoCollection ^. levels
    , _viewState = ViewState False [] S.empty False False "" False
    }

spec :: Spec
spec = do
  describe "mouse click interpretation" $ do
    it "worker move" $ do
      render gs
      second (^. viewState) (interpretClick gs (mouse 1 1)) `shouldBe` (Nothing, gs ^. viewState)
      second (^. viewState) (interpretClick gs (mouse 3 1)) `shouldBe`
        (Just (A.MoveWorker (Point 3 1)), gs ^. viewState)
      second (^. viewState) (interpretClick gs (mouse 1 2)) `shouldBe`
        (Just (A.MoveWorker (Point 1 2)), gs ^. viewState)
      let vs = gs ^. viewState
      let (a1, gs1) = interpretClick gs (mouse 2 2)
      let (a2, gs2) = interpretClick gs1 (mouse 2 3)
      (a1, gs1 ^. viewState) `shouldBe` (Just (A.SelectBox (Point 2 2)), vs & clicks .~ [Point 2 2])
      (a2, gs2 ^. viewState) `shouldBe` (Just (A.MoveBoxes [Point 2 2] [Point 2 3]), vs & clicks .~ [])
    it "1 box move" $ gs `shouldBe` gs
  describe "path finding" $ do
    it "move to (2, 1)" $ do
      let src = gs ^. levelState . worker
      let dst = Point 2 1
      aStarTest src dst `shouldBe` [Point 5 3, Point 5 2, Point 4 2, Point 3 2, Point 3 1, Point 2 1]
    it "move to self" $ do
      let src = gs ^. levelState . worker
      let dst = src
      aStarTest src dst `shouldBe` [Point 5 3]
    it "move to inaccessible area" $ do
      let src = gs ^. levelState . worker
      let dst = Point 1 2
      aStarTest src dst `shouldBe` []
  describe "path conversion" $ do
    it "convert normal" $ do
      let points = [Point 5 3, Point 5 2, Point 4 2, Point 4 1, Point 3 1, Point 2 1]
      pathToDirections points `shouldBe` [L, U, L, U, U]
    it "convert with gap" $ do
      let points = [Point 5 3, Point 5 2, Point 3 1, Point 4 1, Point 3 1, Point 2 1]
      pathToDirections points `shouldBe` [L]
    it "convert single point" $ do
      let points = [Point 5 3]
      pathToDirections points `shouldBe` []
    it "convert empty" $ do
      let points = []
      pathToDirections points `shouldBe` []
  describe "compute move access area" $ do
    it "bottom left area" $ do
      let area = S.fromList [Point 5 3,Point 5 2,Point 4 1,Point 4 2,Point 2 1,Point 3 1,Point 5 4,Point 3 2]
      S.fromList (breadFirstTest (Point 5 3)) `shouldBe` area
    it "convert normal" $ do
      let area = S.fromList [Point 1 5,Point 1 4,Point 1 3,Point 1 2,Point 3 5,Point 2 3,Point 3 4,Point 2 5]
      S.fromList (breadFirstTest (Point 1 2)) `shouldBe` area

  describe "unboxing Cell" $ do
    it "unbox Cell (to . from)" $ do
      let cells =
            [ Worker U
            , Worker D
            , Worker L
            , Worker R
            , WorkerOnGoal U
            , WorkerOnGoal D
            , WorkerOnGoal L
            , WorkerOnGoal R
            , Goal
            , Box
            , BoxOnGoal
            , Empty
            , Wall
            ]
      map (toCell . fromCell) cells `shouldBe` cells
    it "unbox Cell (from . to)" $ do
      let codes = [4, 5, 6, 7, 12, 13, 14, 15, 8, 1, 9, 0, 16]
      map (fromCell . toCell) codes `shouldBe` codes

  where
    mouse (i :: Int) (j :: Int) = "\ESC[<0;" <> show (j * 2 + 3) <> ";" <> show (i + 2) <> "m"
    breadFirstTest src = runIdentity $ breadFirstFind solver src
    aStarTest src dst = runIdentity $ aStarFind solver src dst (return . (== dst))
    solver = AStarSolver {neighbors = neighbors, distance = distance, heuristic = heuristic, projection = p2i, nodesBound = m * n, unproject = i2p}
    m = gs ^. levelState . height
    n = gs ^. levelState . width
    p2i (Point i j) = i * n + j
    i2p k = Point (k `div` n)  (k `mod` n)
    heuristic (Point i1 j1) (Point i2 j2) = return $ abs (i1 - i2) + abs (j1 - j2)
    distance np p0 = fromEnum (np /= p0)
    neighbors p0 =
      return $ do
        let isAccessible p = evalState (isEmptyOrGoal <$> getCell p) gs
        let neighs = map (movePoint p0) [U, D, L, R]
        filter isAccessible neighs

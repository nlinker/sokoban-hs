{-# LANGUAGE ScopedTypeVariables #-}
module Sokoban.ModelSpec where

import Control.Arrow          (second)
import Control.Lens           ((&), (.~), (^.))
import Control.Monad.Identity (runIdentity)
import Control.Monad.State    (evalState)
import Data.Maybe             (fromJust)
import Sokoban.Console        (interpretClick, render)
import Sokoban.Level          (Direction(..), Point(..), isEmptyOrGoal, levels, movePoint)
import Sokoban.Model          (GameState(..), ViewState(..), clicks, getCell, initial, levelState,
                               viewState, worker)
import Sokoban.Resources      (yoshiroAutoCollection)
import Sokoban.Solver         (AStarSolver(..), aStarFind, pathToDirections)

import qualified Data.HashSet  as S
import qualified Sokoban.Model as A (Action(..))

import Test.Hspec

gs :: GameState
gs =
  GameState
    { _collection = yoshiroAutoCollection
    , _index = 0
    , _levelState = fromJust $ initial $ head $ yoshiroAutoCollection ^. levels
    , _viewState = ViewState False [] S.empty False False
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
      (a1, gs1 ^. viewState) `shouldBe` (Nothing, vs & clicks .~ [Point 2 2])
      (a2, gs2 ^. viewState) `shouldBe` (Just (A.MoveBoxes [Point 2 2] [Point 2 3]), vs & clicks .~ [])
    it "1 box move" $ gs `shouldBe` gs
  describe "path finding" $ do
    it "move to (2, 1)" $ do
      let src = gs ^. levelState . worker
      let dst = Point 2 1
      aStarTest src dst `shouldBe` [Point 5 3, Point 5 2, Point 4 2, Point 4 1, Point 3 1, Point 2 1]
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
  where
    mouse (i :: Int) (j :: Int) = "\ESC[<0;" <> show (j * 2 + 1) <> ";" <> show (i + 2) <> "m"
    aStarTest src dst = runIdentity $ aStarFind solver src dst
    solver = AStarSolver {neighbors = neighbors, distance = distance, heuristic = heuristic}
    heuristic (Point i1 j1) (Point i2 j2) = abs (i1 - i2) + abs (j1 - j2)
    distance np p0 = return $ fromEnum (np /= p0)
    neighbors p0 =
      return $ do
        let isAccessible p = evalState (isEmptyOrGoal <$> getCell p) gs
        let neighs = map (movePoint p0) [U, D, L, R]
        filter isAccessible neighs
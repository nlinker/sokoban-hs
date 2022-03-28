module Sokoban.ModelSpec where

import Control.Lens ((^.), (&), (.~))
import Sokoban.Model   (GameState(..), initial, Point(..), ViewState(..), viewState, clicks)
import Sokoban.Resources (yoshiroAutoCollection)
import Sokoban.Level (levels)
import Data.Maybe (fromJust)
import Sokoban.Console (render, interpretClick)
import Test.Hspec

import qualified Data.HashSet  as S
import qualified Sokoban.Model as A (Action(..))
import Control.Arrow (second)

main :: IO ()
main = hspec spec

gs :: GameState
gs =
  GameState
    { _collection = yoshiroAutoCollection
    , _index = 0
    , _levelState = fromJust $ initial $ head $ yoshiroAutoCollection ^. levels
    , _viewState = ViewState [] S.empty
    }

spec :: Spec
spec =
  describe "mouse click interpretation" $ do
    it "worker move" $ do
      render gs
      second (^. viewState) (interpretClick gs (mouse 1 1)) `shouldBe` (Nothing, gs ^. viewState)
      second (^. viewState) (interpretClick gs (mouse 3 1)) `shouldBe` (Just (A.MoveWorker (Point 3 1)), gs ^. viewState)
      second (^. viewState) (interpretClick gs (mouse 1 2)) `shouldBe` (Just (A.MoveWorker (Point 1 2)), gs ^. viewState)

      let vs = gs ^. viewState
      let (a1, gs1) = interpretClick gs (mouse 2 2)
      let (a2, gs2) = interpretClick gs1 (mouse 2 3)
      (a1, gs1 ^. viewState) `shouldBe` (Nothing, vs & clicks .~ [Point 2 2])
      (a2, gs2 ^. viewState) `shouldBe` (Just (A.MoveBoxes [Point 2 2] [Point 2 3]), vs & clicks .~ [])

    it "1 box move" $ gs `shouldBe` gs

  where
    -- in sync with extractMouseClick
    mouse i j = "\ESC[<0;" <> show (j * 2 + 1) <> ";" <> show (i + 2) <> "m"
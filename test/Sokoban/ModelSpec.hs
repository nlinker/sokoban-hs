module Sokoban.ModelSpec where

import Control.Lens ((^.))
import Sokoban.Model   (GameState(..), initial, interpretClick, Point(..))
import Sokoban.Resources (microbanCollection)
import Sokoban.Level (levels)
import Data.Maybe (fromJust)
import Sokoban.Console (render)
import Test.Hspec

import qualified Sokoban.Model as A (Action(..))

main :: IO ()
main = hspec spec

gs :: GameState
gs =
  GameState
    { _collection = microbanCollection
    , _index = 0
    , _levelState = fromJust $ initial $ (microbanCollection ^. levels) !! 13
    }

spec :: Spec
spec =
  describe "mouse click interpretation" $ do
    it "worker move" $ do
      render gs
      interpretClick gs (Point 1 1, False)  `shouldBe` (Just $ A.MoveWorker $ Point 1 1)

    it "1 box move" $
      gs `shouldBe` gs

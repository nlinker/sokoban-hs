module Sokoban.ModelSpec where

import Control.Lens ((^.))
import Sokoban.Model   (GameState(..), initial, interpretClick, Point(..))
import Sokoban.Resources (yoshiroAutoCollection)
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
    { _collection = yoshiroAutoCollection
    , _index = 0
    , _levelState = fromJust $ initial $ head $ yoshiroAutoCollection ^. levels
    }

spec :: Spec
spec =
  describe "mouse click interpretation" $ do
    it "worker move" $ do
      render gs
      interpretClick gs (Point 1 1)  `shouldBe` Nothing
      interpretClick gs (Point 2 1)  `shouldBe` (Just $ A.MoveWorker $ Point 2 1)
      interpretClick gs (Point 1 2)  `shouldBe` (Just $ A.MoveWorker $ Point 1 2)

    it "1 box move" $
      gs `shouldBe` gs

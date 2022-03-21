{-
import Sokoban.Model       (GameState, Point(..), cells, height, initial, name, step, width, holes, boxes, isComplete, extractWBH)
import Sokoban.Parser      (parseLevel, rawLevel, rawLevelSimple)
import qualified Data.Vector   as V

c0 = V.fromList $ V.fromList <$>
  [ [Empty, Empty, Wall, Wall, Wall, Wall, Wall]
  , [Empty, Wall, Wall, Empty, BoxOnHole, Empty, Wall]
  , [Empty, Wall, Empty, WorkerOnHole L, Empty, Empty, Wall]
  , [Wall, Wall, Empty, Wall, Hole, Hole, Wall]
  , [Wall, Empty, Empty, Box, Hole, Empty, Wall]
  , [Wall, Empty, Box, Box, Empty, Box, Wall]
  , [Wall, Wall, Wall, Empty, Empty, Empty, Wall]
  , [Empty, Empty, Wall, Wall, Wall, Wall, Wall]
  ]

triple = extractWBH c0


xxs :: Vector (Vector Integer)
xxs = V.fromList $ V.fromList <$> [[i * 10 + j | j <- [1 .. 4]] | i <- [1 .. 3]]

extract :: Integral a => Vector (Vector a) -> (Maybe (Int, Int), S.HashSet (Int, Int))
extract xs =
  (flip execState) (Nothing, S.empty) $ do
    let m = V.length xs
    let n = V.length (xs ! 0)
    forM_ [0 .. m - 1] $ \i ->
      forM_ [0 .. n - 1] $ \j -> do
        let x = (xs ! i) ! j
        when (x == 23) $ _1 .= Just (i, j)
        when (x `mod` 2 == 0) $ do
          stuff <- use _2
          _2 .= S.insert (i, j) stuff
    return ()

getWorkerDirection :: Cell -> Direction
getWorkerDirection c =
    case c of
        Worker d       -> d
        WorkerOnHole d -> d
        _              -> D

-- debug output for undoStack
diffs <- use undoStack
let space = (replicate 80 ' ') <> "\n"
let msg = concatMap (\x -> show x <> "\n") diffs <> space
name .= T.pack msg
-}

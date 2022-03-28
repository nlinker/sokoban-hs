{-
  case ... of
    _ | trace (show $ T.pack (show $ extractMouseClick key)) False -> undefined
    ...

  levelState . message %=
    (\msg ->
       let nm = T.length msg
           msg1 = "action = " <> show action
        in T.pack (msg1 <> replicate (nm - length msg1) ' '))
-}

-- debug example

--  let hs = gs ^. goals
--  let bs = gs ^. boxes
--  let isc = gs ^. isComplete
--  putStrLn $ "hs = " <> show hs <> " bs = " <> show bs <> " isComplete = " <> show isc
--  forM_ [0 .. gs ^. height + 21] $ \_ -> putStr "\ESC[A"
-- gs & levelState . message .~ T.pack (show c)


{-
import Sokoban.Model       (GameState, Point(..), cells, height, initial, name, step, width, goals, boxes, isComplete, extractWBH)
import Sokoban.Parser      (parseLevel, rawLevel, rawLevelSimple)
import qualified Data.Vector   as V

c0 = V.fromList $ V.fromList <$>
  [ [Empty, Empty, Wall, Wall, Wall, Wall, Wall]
  , [Empty, Wall, Wall, Empty, BoxOnGoal, Empty, Wall]
  , [Empty, Wall, Empty, WorkerOnGoal L, Empty, Empty, Wall]
  , [Wall, Wall, Empty, Wall, Goal, Goal, Wall]
  , [Wall, Empty, Empty, Box, Goal, Empty, Wall]
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
        WorkerOnGoal d -> d
        _              -> D

-- debug output for undoStack
diffs <- use undoStack
let space = (replicate 80 ' ') <> "\n"
let msg = concatMap (\x -> show x <> "\n") diffs <> space
name .= T.pack msg


# in the current copy of the repo
git filter-branch --tree-filter "rm -rf haskell/sokoban" --prune-empty HEAD
git for-each-ref --format="%(refname)" refs/original/ | xargs -n 1 git update-ref -d

# in another clone of the repo
# separate subdir into separate repo
git filter-branch --prune-empty --subdirectory-filter haskell/sokoban master


  # Filter the specified branch in your directory and remove empty commits
  > Rewrite 48dc599c80e20527ed902928085e7861e6b3cbe6 (89/89)
  > Ref 'refs/heads/BRANCH-NAME' was rewritten


import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

main = do
    ls <- fmap Text.lines (Text.readFile "filename.txt")


stack runghc --package brick MouseDemo.hs



M C b C x C y
C x and C y are the x and y coordinates of the mouse event, encoded as in X10 mode.

upper left
[27,91,60,48,59,49,59,49,109]
--->
[27,91,60,48,59,57,59,49,109]
--->
[27,91,60,48,59,49,48,59,49,109]
--->
[27,91,60,48,59,49,57,59,50,109]
[27,91,60,48,59,50,48,59,50,109]


bottom left
[27,91,60,48,59,49,59,51,51,109]

upper right
[27,91,60,48,59,49,50,53,59,49,109]

bottom right
[27,91,60,48,59,49,50,53,59,51,51,109]

readMaybe

-}

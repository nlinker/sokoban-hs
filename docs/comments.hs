{-

aStarFind :: MonadState GameState m => Point -> Point -> m AStarData
aStarFind src dst = do
  let ad0 = initAStarData src
  ad1 <- aStarFindRec ad0
  ad2 <- aStarFindRec ad1
  ad3 <- aStarFindRec ad2
  ad4 <- aStarFindRec ad3
  ad5 <- aStarFindRec ad4
  traceM $ showAsd ad0
  traceM $ showAsd ad1
  traceM $ showAsd ad2
  traceM $ showAsd ad3
  traceM $ showAsd ad4
  traceM $ showAsd ad5
  return $ (\a -> a) ad5

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

An approximation algorithm for minimizing a supermodular set function over comatroids

lookupRow :: Int -> BorderMap a -> IMap a
lookupRow row m
    | row == eTop    (coordinates m) = eTop    (_values m)
    | row == eBottom (coordinates m) = eBottom (_values m)
    | otherwise = IM.fromList
        $  [(eLeft   (coordinates m), Run 1 a) | Just a <- [IM.lookup row (eLeft   (_values m))]]
        ++ [(eRight  (coordinates m), Run 1 a) | Just a <- [IM.lookup row (eRight  (_values m))]]


listRemove :: (Splittable t, Foldable t, Semigroup (t e))
           => Int
           -- ^ The position at which to remove an element (0 <= i <
           -- size)
           -> GenericList n t e
           -> GenericList n t e
listRemove pos l | null l = l
                 | pos /= splitClamp l pos = l
                 | otherwise =
    let newSel = case l^.listSelectedL of
            Nothing -> 0
            Just s | pos == 0 -> 0
                   | pos == s -> pos - 1
                   | pos  < s -> s - 1
                   | otherwise -> s
        (front, rest) = splitAt pos es
        (_, back) = splitAt 1 rest
        es' = front <> back
        es = l^.listElementsL
    in l & listSelectedL .~ (if null es' then Nothing else Just newSel)
         & listElementsL .~ es'


prettyFileSize :: Int64
               -- ^ A file size in bytes.
               -> T.Text
prettyFileSize i
    | i >= 2 ^ (40::Int64) = T.pack $ format (i `divBy` (2 ** 40)) <> "T"
    | i >= 2 ^ (30::Int64) = T.pack $ format (i `divBy` (2 ** 30)) <> "G"
    | i >= 2 ^ (20::Int64) = T.pack $ format (i `divBy` (2 ** 20)) <> "M"
    | i >= 2 ^ (10::Int64) = T.pack $ format (i `divBy` (2 ** 10)) <> "K"
    | otherwise    = T.pack $ show i <> " bytes"
    where
        format = printf "%0.1f"
        divBy :: Int64 -> Double -> Double
        divBy a b = ((fromIntegral a) :: Double) / b

toCurrent :: Range -> T.Text -> Position -> Maybe Position
toCurrent (Range (Position startLine startColumn) (Position endLine endColumn)) t (Position line column)
    | line < startLine || line == startLine && column < startColumn =
      -- Position is before the change and thereby unchanged.
      Just $ Position line column
    | line > endLine || line == endLine && column >= endColumn =
      -- Position is after the change so increase line and column number
      -- as necessary.
      Just $ Position (line + lineDiff) newColumn
    | otherwise = Nothing
    -- Position is in the region that was changed.
    where
        lineDiff = linesNew - linesOld
        linesNew = T.count "\n" t
        linesOld = endLine - startLine
        newEndColumn
          | linesNew == 0 = startColumn + T.length t
          | otherwise = T.length $ T.takeWhileEnd (/= '\n') t
        newColumn
          | line == endLine = column + newEndColumn - endColumn
          | otherwise = column

fromCurrent :: Range -> T.Text -> Position -> Maybe Position
fromCurrent (Range (Position startLine startColumn) (Position endLine endColumn)) t (Position line column)
    | line < startLine || line == startLine && column < startColumn =
      -- Position is before the change and thereby unchanged
      Just $ Position line column
    | line > newEndLine || line == newEndLine && column >= newEndColumn =
      -- Position is after the change so increase line and column number
      -- as necessary.
      Just $ Position (line - lineDiff) newColumn
    | otherwise = Nothing
    -- Position is in the region that was changed.
    where
        lineDiff = linesNew - linesOld
        linesNew = T.count "\n" t
        linesOld = endLine - startLine
        newEndLine = endLine + lineDiff
        newEndColumn
          | linesNew == 0 = startColumn + T.length t
          | otherwise = T.length $ T.takeWhileEnd (/= '\n') t
        newColumn
          | line == newEndLine = column - (newEndColumn - endColumn)
          | otherwise = column



Как вы избавляетесь от дублирования в
```
data Foo c d
  = A
  | B Int
  | C c
  | D d
  | E c d
  | F Int c d

getMagic :: Foo c d -> Int
getMagic :: Foo c d -> Int
getMagic A            = 1
getMagic (B _i)       = 2
getMagic (C _c)       = 2
getMagic (D _d)       = 3
getMagic (E _c _d)    = 3
getMagic (F _i _c _d) = 3
```

Выражения 1, 2 и 3 могут быть большие

youtube-dl -a urls.txt --write-thumbnail --skip-download

-}


{--
  case buildPath (pd1 ^. cameFrom) of
    Nothing   -> return Nothing
    Just path -> return $ Just path

aStarFind :: MonadState GameState m => Point -> Point -> m AStarData
aStarFind src dst = do
  let ad0 = initAStarData src
  ad1 <- aStarFindRec ad0
  ad2 <- aStarFindRec ad1
  ad3 <- aStarFindRec ad2
  ad4 <- aStarFindRec ad3
  ad5 <- aStarFindRec ad4
  traceM $ showAsd ad0
  traceM $ showAsd ad1
  traceM $ showAsd ad2
  traceM $ showAsd ad3
  traceM $ showAsd ad4
  traceM $ showAsd ad5
  return $ (\a -> a) ad5
  where
    showAsd ad =
      let frontierS = show $ Q.toList $ ad ^. frontier
          cameFromS = show $ H.toList $ ad ^. cameFrom
          costSoFarS = show $ H.toList $ ad ^. costSoFar
       in frontierS <> "; " <> cameFromS <> "; " <> costSoFarS
    aStarFindRec :: MonadState GameState m => AStarData -> m AStarData
    aStarFindRec asd0 =
      case Q.findMin (asd0 ^. frontier) of
        Nothing -> return asd0
        Just (k, _, ())
          | k == dst -> return asd0
        Just (k, _, ()) -> do
          let ns = map (moveDir k) [U, D, L, R]
          ns2 <- mapM getCell ns
          let neighbors = map fst $ filter (\(_, c) -> isEmptyOrGoal c) $ zip ns ns2
          let currentCost = (asd0 ^. costSoFar) H.! k
          let asd1 =
                flip execState asd0 $ forM_ neighbors $ \np -> do
                  let newCost = currentCost + 1
                  csf <- use costSoFar
                    -- if next not in cost_so_far or new_cost < cost_so_far[next]:
                  when (not (H.member np csf) || newCost < csf H.! np) $ do
                    costSoFar .= H.insert np newCost csf
                    let priority = newCost + heuristic np dst
                    frontier %= Q.insert np priority ()
                    cameFrom %= H.insert np (Just k)
          -- findPathRec pathDataNew
          return asd1

buildPath :: H.HashMap Point (Maybe Point) -> Maybe [Point]
buildPath = undefined


 levelState . message .= T.pack ("(" <> show src <> " -> " <> show dst <> ")")


calculateAndMoveWorker :: MonadState GameState m => Point -> m ()
calculateAndMoveWorker dst = do
  src <- use $ levelState . worker
  gs <- get
  path <- aStarFind src dst isAccessible -- ?????????????
  levelState . message .= T.pack ("(" <> show src <> " -> " <> show dst <> "): " <> show path <> "      ")
  where
    isAccessible :: MonadState GameState m => Point -> m Bool
    isAccessible p = isEmptyOrGoal <$> getCell p

-}

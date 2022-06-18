{-
~/h/sokoban-hs ❯❯❯ stack build --work-dir .stack-work-profile --profile
~/h/sokoban-hs ❯❯❯ stack --profile --work-dir .stack-work-profile run sokoban --rts-options -p

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

doMove :: MonadState GameState m => Direction -> m (Maybe Diff)
doMove d = runDoMove -- execState undoMoveM gameState
  where
    runDoMove = do ...

undoMove :: MonadState GameState m => Diff -> m ()
undoMove diff = runUndoMove -- execState undoMoveM gameState
  where
    runUndoMove = do ...

-- now update the undo stack
-- this variant makes redirections also to be recorded and undoable
-- > let diff = Diff {_point = point0, _direction = d, _cell0 = c0, _cell1 = c1, _cell2 = c2}
-- > when ((d0, d1, d2) /= (c0, c1, c2)) $ undoStack %= (diff :)

runTest :: IO ()
runTest = do
  gs <- buildGameState []
  let src = gs ^. levelState . worker
  let dst = Point 2 1
  let neighbors p0 =
        return $ do
          let isAccessible p = evalState (isEmptyOrGoal <$> getCell p) gs
          let neighs = map (movePoint p0) [U, D, L, R]
          filter isAccessible neighs
  let distance np p0 = return $ fromEnum (np /= p0)
  let heuristic (Point i1 j1) (Point i2 j2) = abs (i1 - i2) + abs (j1 - j2)
  let solver = AStarSolver {neighbors = neighbors, distance = distance, heuristic = heuristic}
  let path = runIdentity $ aStarFind solver src dst
  clearScreen
  _ <- moveWorker gs path
  print path
  where
    moveWorker :: GameState -> [Point] -> IO GameState
    moveWorker gs1 [] = return gs1
    moveWorker gs1 (p:ps) = do
      let w = gs1 ^. levelState . worker
      let gs2 =
            case deriveDir w p of
              Just U -> step gs1 A.Up
              Just D -> step gs1 A.Down
              Just L -> step gs1 A.Left
              Just R -> step gs1 A.Right
              _      -> gs1
      moveCursorToOrigin
      render gs2
      threadDelay 100000
      moveWorker gs2 ps

--      let neighs = filter (not . (`H.member` closedList0)) $ map (movePoint p0) [U, D, L, R]
--      neighbors <- filterM (lift . isAccessible) neighs

[ [Empty, Wall,  Wall,      Wall,     Wall,  Wall,  Wall]
, [Wall,  Wall,  Empty,     Empty,    Goal,  Empty, Wall]
, [Wall,  Empty, BoxOnGoal, Empty,    Wall,  Empty, Wall]
, [Wall,  Empty, Goal,      Box,      Empty, Empty, Wall]
, [Wall,  Empty, Worker     U, Wall,  Box,   Wall,  Wall]
, [Wall,  Wall,  Empty,     Worker L, Empty, Wall,  Empty]
, [Empty, Wall,  Wall,      Wall,     Wall,  Wall,  Empty]
]

_flatCells = [
  Empty, Wall,  Wall,      Wall,     Wall,  Wall,  Wall,
  Wall,  Wall,  Empty,     Empty,    Goal,  Empty, Wall,
  Wall,  Empty, BoxOnGoal, Empty,    Wall,  Empty, Wall,
  Wall,  Empty, Goal,      Box,      Empty, Empty, Wall,
  Wall,  Empty, Empty,     Wall,     Box,   Wall,  Wall,
  Wall,  Wall,  Empty,     Worker D, Empty, Wall,  Empty,
  Empty, Wall,  Wall,      Wall,     Wall,  Wall,  Empty
],
_flatHeight = 7,
_flatWidth = 7,
_flatWorker = 38,
_flatBoxes = [16,24,32],
_flatGoals = [11,16,23]}

showInMessage :: Show a => GameState -> a -> GameState
showInMessage gs x =
  let nm = T.length $ gs ^. viewState . message
      msg1 = "action = " <> show x
   in gs & viewState . message .~ T.pack (msg1 <> replicate (nm - length msg1) ' ')

------------------------------------
-- an example of using hashtable  --
import Control.Monad.ST    (ST, runST)
import Data.Hashable       (Hashable(..))
import qualified Data.HashTable.Class        as H
import qualified Data.HashTable.ST.Cuckoo    as CuckooHash
type Hashtable s k v = CuckooHash.HashTable s k v

instance (VU.Unbox a, Hashable a) => Hashable (VU.Vector a) where
  hashWithSalt salt = hashWithSalt salt . VU.toList
  {-# INLINE hashWithSalt #-}

-- MV.STVector s Int
makeHT :: ST s (Hashtable s (VU.Vector Int) Int)
makeHT = do
  ht <- H.new
  forM_ [0 .. 9] $ \key -> do
    let vec = VU.replicate 10 0
    vec0 <- VU.thaw vec
    VM.write vec0 key (key + 1)
    vecf <- VU.freeze vec0
    H.mutate ht vecf $ \case
      Nothing -> (Just 0, ())
      Just _ -> (Just 0, ())
  pure ht

makeAssocList :: [(VU.Vector Int, Int)]
makeAssocList =
  runST $ do
    ht <- makeHT
    H.toList ht

aStarFind :: (Monad m, Hashable p, Ord p) => AStarSolver m p -> p -> p -> m [p]
aStarFind solver src dst = do
  let astar = aStarInit src
  evalStateT (aStarFindRec solver dst) astar

import Text.InterpolatedString.QM (qm)

viewState . message %= (\msg -> T.replicate (T.length msg) " ")

aStarInitST :: (PrimMonad m, Ord p) => p -> (p -> Int) -> m (AStar (PrimState m) p)
aStarInitST src p2i = do
  heap <- HMD.new 1000000
  HMD.unsafePush (mempty :: Min) (p2i src) heap
  closedList <- HM.newSized 1000000
  return $ AStar { _heap = heap, _closedList = closedList }

aStarInit :: (Hashable p, Ord p) => p -> AStar p
aStarInit src =
  let weight = Weight {_fScore = 0, _gScore = 0, _parent = src}
      openList = Q.singleton src (weight ^. fScore) weight
      closedList = H.empty :: H.HashMap p p
   in AStar openList closedList

backtrace :: (Eq p, Hashable p) => p -> H.HashMap p p -> [p]
backtrace dst closedList = backtraceRec dst [dst]
  where
    backtraceRec current acc = -- we repeatedly lookup for the parent of the current node
      case H.lookup current closedList of
        Nothing -> []
        Just parent
          | current == parent -> acc
        Just parent -> backtraceRec parent (parent : acc)

breadFirstInit :: (Hashable p, Ord p, Show p) => p -> BreadFirst p
breadFirstInit src = do
  let weight = Weight {_fScore = 0, _gScore = 0, _parent = src}
      openList = Q.singleton src (weight ^. fScore) weight
      closedList = H.empty :: H.HashMap p ()
   in BreadFirst openList closedList

breadFirstFindRec :: (Monad m, Hashable p, Ord p, Show p) => AStarSolver m p -> StateT (BreadFirst p) m [p]
breadFirstFindRec solver = do
  openList0 <- use openListB
  closedList0 <- use closedListB
  case Q.findMin openList0 of
    Nothing -> do
      return $ H.keys closedList0
    Just (p0, _, weight0) -> do
          openListB %= Q.delete p0
          closedListB %= H.insert p0 ()
          neighbors <- lift $ neighbors solver p0
          let neighPoints = filter (not . (`H.member` closedList0)) neighbors
          -- `k` is the current node, `fs` is f-score
          forM_ neighPoints $ \np -> do
            let dist = distance solver np p0
            let g1 = weight0 ^. gScore + dist
            let f1 = g1
            let p1 = p0
            let w1 = Weight {_fScore = f1, _gScore = g1, _parent = p1}
            case Q.lookup np openList0 of
              Just (_, w)
                  -- the neighbour can be reached with smaller cost - change priority
                  -- otherwise don't touch the neighbour, it will be taken by open_list.pop()
               -> when (g1 < (w ^. gScore)) $ openListB .= Q.insert np f1 w1 openList0
              Nothing
                -- the neighbour is new
               -> openListB .= Q.insert np f1 w1 openList0
          breadFirstFindRec solver

traceM [qm| {unproject solver . snd <$> top'} // {top'}|]
ol <- keyValues openList
traceM [qm| __ openList: {ol}|]
cl <- keyValues closedList
traceM [qm| __ closedList: {cl}|]

keyValues :: (Monad m, Hashable k, Eq k) => HM.MHashMap s k v -> STT s m [(k, v)]
keyValues hm = HM.foldM (\a k v -> return $ (k, v) : a) [] hm


flattenLevel :: GameState -> FlatLevelState
flattenLevel gs =
  runST $ do
    vec <- VU.thaw $ VU.replicate (m * n) Empty
    forM_ [0 .. m - 1] $ \i ->
      forM_ [0 .. n - 1] $ \j -> do
        let c = (cs ! i) ! j
        VM.write vec (i * n + j) c
    flatCells <- VU.freeze vec
    let (w, bz, gz) = fromMaybe (error $ "Invariant violation" <> show (gs ^. levelState)) (extractWBH cs)
    let flatBoxes = VU.fromList $ sort $ map p2f $ S.toList bz
    let flatGoals = VU.fromList $ sort $ map p2f $ S.toList gz
    return $
      FlatLevelState
        { _flatCells = flatCells
        , _flatHeight = m
        , _flatWidth = n
        , _flatWorker = p2f w
        , _flatBoxes = flatBoxes
        , _flatGoals = flatGoals
        }
  where
    m = gs ^. levelState . height
    n = gs ^. levelState . width
    cs = gs ^. levelState . cells
    p2f (Point i j) = i * n + j
    -- f2p k = Point (k `div` n) (k `mod` n)

unFlattenLevel :: FlatLevelState -> LevelState
unFlattenLevel _fls = undefined

-- Add stm-containers to dependencies
stack.yaml:
-----------
extra-deps:
  - cabal-info-0.1.0.0
  - impure-containers-0.5.0
  - stm-containers-1.1.0.4
  - stm-hamt-1.2.0.4

package.yaml:
-------------
dependencies:
    - stm-containers

-- HM.mapM_ (\k v -> traceM [qm|k={k} v={v}|]) closedList

buildPush2Solver :: forall m . MonadState GameState m => m (AStarSolver m PPDD)
buildPush2Solver = do
  let p2int _ppdd = undefined
  let int2p _k = undefined
  let neighbors (p0 :: PPDD) = do
        _moveSolver <- buildMoveSolver [p0 ^. pointFst, p0 ^. pointSnd] :: m (AStarSolver m Point)
        let isAccessible :: PPDD -> m Bool
            isAccessible p = do
              c1 <- getCell (p ^. pointFst)
              c2 <- getCell (p ^. pointSnd)
              return $ isEmptyOrGoal c1 && isEmptyOrGoal c2
--        let tryBuildPath :: MonadState GameState m => Point -> Point -> m [Point]
--            tryBuildPath src dst = do
--              accessible <- isAccessible dst
--              if accessible
--                then pathToDirections <$> aStarFind moveSolver src dst (return . (== dst))
--                else return []
          -- cont is the "continue push in the direction d0" neighbor
          -- src is the position of the worker for the push
          -- directed is the same box but with changed push direction
        let p1 = p0 ^. pointFst
        let d1 = p0 ^. dirFst
        let p2 = p0 ^. pointSnd
        let d2 = p0 ^. dirSnd
--        cont1 <- filterM (\(PPDD p _ _ _ _) -> isAccessible p) [PPDD (movePoint p0 d0) d0 [d0]]
--        cont2 <- filterM (\(PPDD p _ _ _ _) -> isAccessible p) [PPDD (movePoint p0 d0) d0 [d0]]
--
--        let src = movePoint p0 (opposite d0)
--        let otherDirs = filter (/= d0) [U, D, L, R]
--        paths <- mapM (\d -> PD p0 d <$> tryBuildPath src (movePoint p0 $ opposite d)) otherDirs
--        (cont <>) <$> filterM (\(PD _ _ ds) -> (return . not . null) ds) paths
        undefined
  let heuristic p1 p2 = undefined -- return $ abs (i1 - i2) + abs (j1 - j2)
  let distance np p0 = undefined -- return $ fromEnum (np /= p0)
  return $ AStarSolver {neighbors = neighbors, distance = distance, heuristic = heuristic, p2int = p2int, int2p = int2p}

  let int2p :: Int -> PD
      int2p k = let kdir = k `mod` 4
                    k4 = k `div` 4
                in PD (Point (k4 `div` n) (k4 `mod` n)) (w8ToDirection (fromIntegral kdir)) []

  -- this is to clear artifacts after the previous message
  msg <- use (viewState . message)
  unless (T.null msg) $ viewState . message .= ""
    -- viewState . doClearScreen .= True
  viewState . message .= T.pack (show action)

dumpState :: MonadState GameState m => m ()
dumpState = do
  undos <- use (levelState . undoStack)
  uidx <- use (levelState . undoIndex)
  let msg1 = "uidx: " <> show uidx <> "\n"
  let msg2 = msg1 <> concatMap (\x -> show x <> "\n") undos
  viewState . message .= T.pack msg2

optimization idea
-----------------

cacheLookup src dst =
  return $ unsafePerformIO $ do
    hm <- readMVar pathCache
    dm <- getDebugModeM
    when dm $ traceM [qm| {(src, dst)} -> {H.lookup (src, dst) hm} // {walls} |]
    return $ H.lookup (src, dst) hm
cacheUpdate src dst path =
  return $ unsafePerformIO $ do
    traceM [qm| cacheUpdate {(src, dst)} -> {path} // {walls} |]
      dm <- getDebugModeM
      traceM [qm| update {(src, dst)} -> {path} // {walls} |]
    modifyMVar_ pathCache (return . H.insert (src, dst) path)

    moveSolverCache :: MVar (H.HashMap Point (AStarSolver m Point))
    moveSolverCache = unsafePerformIO $ newMVar H.empty
    {-# NOINLINE moveSolverCache #-}

    withCache p builder = do
      let path' =
            unsafePerformIO $ do
              hm <- readMVar moveSolverCache
              return $ H.lookup p hm
      case path' of
        Just path -> return path
        Nothing -> do
          solver <- builder
          return $ unsafePerformIO $
            hm <- readMVar moveSolverCache
            dm <- getDebugModeM
            when dm $ traceM [qm| withCache moveSolverCache {p}: size={H.size hm} |]
            modifyMVar moveSolverCache (\hm -> return (H.insert p solver hm, solver))

    pathCache :: MVar (H.HashMap (Point, Point) [Point])
    {-# NOINLINE pathCache #-}
    pathCache = unsafePerformIO $ newMVar H.empty

    withCache src dst algorithm = do
      let path' =
            unsafePerformIO $ do
              hm <- readMVar pathCache
              return $ H.lookup (src, dst) hm
      case path' of
        Just path -> return path
        Nothing -> do
          path <- algorithm
          return $ unsafePerformIO $
            dm <- getDebugModeM
            hm <- readMVar pathCache
            when dm $ traceM [qm| cacheUpdate {(src, dst)} -> {path}: {walls} size={H.size hm} |]
            modifyMVar pathCache (\hm -> return (H.insert (src, dst) path hm, path))

instance (Monad m) => PrimMonad (STT s m) where
  type PrimState (STT s m) = s
  primitive f =
    STT $ \s ->
      case f s of
        (# t, a #) -> return (STTRet t a)
  {-# INLINE primitive #-}

trace :: String -> String -> IO ()
trace act sql =
  do t <- tracingEnabled
   when t $ do
      let s = act ++ ": " ++ sql
            mf <- traceFile
            case mf of
              Nothing -> hPutStrLn stderr s
              Just f  -> appendFile f s

runTest :: IO ()
runTest = do
  gs <- buildGameState []
  render gs
  let x =
        runST $ do
          mc <- HM.new
          pc <- HM.new
          let ctx = SolverContext mc pc (gs ^. levelState . height) (gs ^. levelState . width)
          solver <- buildMoveSolver ctx []
          area <- evalStateT (breadFirstFind solver (Point 2 1)) gs
          traceM [qm| area={area} |]
          return area
  putStrLn [qm| x={x} |]

  let areas =
        runST $ do
          pc <- HM.new
          let ctx = SolverContext pc m n
          forM sources $ \src -> do
            pushSolver <- buildPushSolver ctx
            area <- flip evalStateT gs $ breadFirstFind pushSolver src
            dm <- getDebugModeM
            when dm $ do
              pcSize <- HM.foldM (\a _ _ -> return $ a + 1) 0 pc
              traceM [qm| pcSize={pcSize} |]
            return area


selector :: Functor f => (Point -> f Point) -> PPD -> f PPD
selector f ppd@(PPD _ _ _ i _) = if i == 0 then ppdFst f ppd else ppdSnd f ppd

neighbors1 :: forall m . PrimMonad m => SolverContext m -> PPD -> StateT GameState m [PPD]
neighbors1 ctx (PPD p1 p2 d i _dirs) = do
  let walls = [p1, p2]
  let myFind src dst = do
        moveSolver <- lift $ buildMoveSolver ctx dst walls
        let pc = ctx ^. pathCache
        path' <- HM.lookup pc (walls, d, src, dst)
        case path' of
          Just path -> return path
          Nothing -> do
            path <- aStarFind moveSolver src
            HM.insert pc (walls, d, src, dst) path
            return path
  let isAccessible p = isEmptyOrGoal <$> getCell p
  let tryBuildPath :: Point -> Point -> StateT GameState m [Direction]
      tryBuildPath src dst = do
        accessible <- isAccessible dst
        if accessible
          then pathToDirections <$> myFind src dst
          else return []
  -- cont is the "continue push in the direction d0" of the current i-th box
  let contPPD = (\p -> p & selector %~ (`movePoint` d)) <$> [PPD p1 p2 d i [d]]
  let points = [p1, p2]
  let curr = points !! i
  let w = movePoint curr $ opposite d

  cont <- filterM (isAccessible . (^. selector)) contPPD
  let neighs1 = nub $ map (movePoint p1) [U, D, L, R] <> map (movePoint p2) [U, D, L, R]
  -- let src = movePoint p0 (opposite d0)
  -- let otherDirs = filter (/= d0) [U, D, L, R]
  -- paths <- mapM (\d -> PD p0 d <$> tryBuildPath src (movePoint p0 $ opposite d)) otherDirs
  -- (cont <>) <$> filterM (\(PD _ _ ds) -> (return . not . null) ds) paths
  return undefined

sources :: [(Direction, Integer)]
ctx :: SolverContext IO
gs, gs3 :: GameState
(gs, gs3, ctx, sources) =
  unsafePerformIO $ do
    gs0 <- (`step` A.NextLevel) <$> buildGameState []
    let gs1 = step gs0 (A.MoveBoxes [Point 7 4] [Point 7 3])
    let gs2 = step gs1 (A.MoveBoxes [Point 6 4] [Point 8 4])
    let gs3 = step gs2 (A.MoveBoxes [Point 7 3] [Point 7 4])
    let gs = eraseBoxes [Point 7 4, Point 8 4] gs3
    ctx <- ctxGs gs
    let part0 = map (, 0) [U, D, L, R]
    let part1 = map (, 1) [U, D, L, R]
    let sources = part0 <> part1
    return (gs, gs3, ctx, sources)
  where
    ctxGs :: PrimMonad m => GameState -> m (SolverContext m)
    ctxGs gs = do
      hm <- HM.new
      let (m, n) = (gs ^. levelState . height, gs ^. levelState . width)
      return $ SolverContext hm m n

let m = gs ^. levelState . height
let n = gs ^. levelState . width
let w = gs ^. levelState . worker

    fromBool :: Bool -> a -> Maybe a
    fromBool p = if p then Just else const Nothing
    tryBuildPath ::
         forall m. (PrimMonad m)
      => SolverContext (StateT GameState m)
      -> Point
      -> Point
      -> StateT GameState m [Point]
    tryBuildPath ctx src dst = do
      moveSolver <- buildMoveSolver ctx dst [box1, box2]
      aStarFind moveSolver src

applyV :: TVar a -> (a -> a) -> IO ()
applyV x fn = atomically $ readTVar x >>= writeTVar x . fn

      let showChan = do
            isEmpty <- atomically $ isEmptyTChan chan
            if isEmpty
              then return ()
              else do
                el <- atomically $ readTChan chan
                putStrLn [qm| el = {el} |]
                showChan
      showChan

newtype App m a = App { runApp :: WriterT [Message] (StateT GameState m) a}
  deriving (Functor, Monad, MonadWriter, MonadState)


# if you want to use reflex add to stack.yaml
#  - reflex-0.6.4
#  - constraints-extras-0.3.0.2
#  - dependent-map-0.3
#  - dependent-sum-0.6.2.0
#  - monoidal-containers-0.6.0.1
#  - patch-0.0.3.1
#  - ref-tf-0.4.0.2
#  - witherable-0.3.1
-}

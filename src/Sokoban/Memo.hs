{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}

module Sokoban.Memo where

import Control.Concurrent.MVar         (MVar, modifyMVar_, newMVar, readMVar)
import Data.Hashable                   (Hashable)
import System.IO.Unsafe                (unsafePerformIO)

import qualified Data.HashMap.Strict        as H
import Text.InterpolatedString.QM (qm)
import Debug.Trace (traceM)

curry2 :: ((a, b) -> c) -> a -> b -> c
curry2 f x y = f (x, y)

uncurry2 :: (a -> b -> c) -> (a, b) -> c
uncurry2 f (x, y) = f x y

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x, y, z)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

curry4 :: ((a, b, c, d) -> e) -> a -> b -> c -> d -> e
curry4 f w x y z = f (w, x, y, z)

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (w, x, y, z) = f w x y z


--makeMemo :: MVar (H.HashMap a b)
--makeMemo = unsafePerformIO $ newMVar H.empty
--{-# NOINLINE makeMemo #-}

memo :: (Hashable a, Eq a, Show a) => MVar (H.HashMap a b) -> (a -> b) -> a -> b
memo cache f x = unsafePerformIO $ do
    m <- readMVar cache
    case H.lookup x m of
      Nothing -> do
        let  r = f x
        modifyMVar_ cache (return . H.insert x r)
        -- traceM [qm| {x} {H.size m}|]
        return r
      Just r  -> return r

memo2 :: (Hashable a, Hashable b, Eq a, Eq b, Show a, Show b) => MVar (H.HashMap (a, b) c) -> (a -> b -> c) -> a -> b -> c
memo2 cache = curry2 . memo cache . uncurry2

memo3 :: (Hashable a, Hashable b, Hashable c, Eq a, Eq b, Eq c, Show a, Show b, Show c) =>
     MVar (H.HashMap (a, b, c) d) -> (a -> b -> c -> d) -> a -> b -> c -> d
memo3 cache = curry3 . memo cache . uncurry3

memo4
  :: (Hashable a, Hashable b, Hashable c, Hashable d, Eq a, Eq b, Eq c, Eq d, Show a, Show b, Show c, Show d) =>
     MVar (H.HashMap (a, b, c, d) e)
     -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> e
memo4 cache = curry4 . memo cache . uncurry4

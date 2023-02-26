{-# LANGUAGE CPP #-}

module Data.HashMap.Mutable.Internal.Array
  ( MutableArray
  , newArray
  , readArray
  , writeArray
  ) where

import Control.Monad.Primitive (PrimMonad, PrimState)

#ifdef BOUNDS_CHECKING
import Data.Vector.Mutable     (MVector)
import qualified Data.Vector.Mutable     as M
#else
import Data.Primitive.Array    (MutableArray)
import qualified Data.Primitive.Array    as M
#endif


-- An MVector is in essence a reference to a vector, you thus need to read
-- to obtain the data contained in the vector. An MVector is also not much
-- more than a wrapper of a MutableArray with two indexes.

#ifdef BOUNDS_CHECKING

type MutableArray s a = MVector s a

newArray :: PrimMonad m => Int -> a -> m (MutableArray (PrimState m) a)
newArray = M.replicate

readArray :: PrimMonad m => MutableArray (PrimState m) a -> Int -> m a
readArray = M.read

writeArray :: PrimMonad m => MutableArray (PrimState m) a -> Int -> a -> m ()
writeArray = M.write

#else

newArray :: PrimMonad m => Int -> a -> m (MutableArray (PrimState m) a)
newArray = M.newArray

readArray :: PrimMonad m => MutableArray (PrimState m) a -> Int -> m a
readArray = M.readArray

writeArray :: PrimMonad m => MutableArray (PrimState m) a -> Int -> a -> m ()
writeArray = M.writeArray

#endif

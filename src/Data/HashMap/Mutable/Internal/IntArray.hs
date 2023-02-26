{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE MagicHash    #-}

module Data.HashMap.Mutable.Internal.IntArray
  ( IntArray
  , Elem
  , elemMask
  , primWordToElem
  , elemToInt
  , elemToInt#
  , newArray
  , readArray
  , writeArray
  , length
  , toPtr
  ) where

------------------------------------------------------------------------------
import           Control.Monad.Primitive  (PrimMonad, PrimState)
import           Data.Bits (FiniteBits(finiteBitSize))
import           GHC.Exts  (Int#, Int(..), Word#, Ptr(..), word2Int#)
import           GHC.Word  (Word16(..))
import           GHC.Prim  (word16ToWord#, wordToWord16#)
import           Prelude hiding (length)
import qualified Data.Primitive.ByteArray as A
------------------------------------------------------------------------------


#ifdef BOUNDS_CHECKING
#define BOUNDS_MSG(sz,i) concat [ "[", __FILE__, ":",                         \
                                  show (__LINE__ :: Int),                     \
                                  "] bounds check exceeded: ",                \
                                  "size was ", show (sz), " i was ", show (i) ]

#define BOUNDS_CHECK(arr,i) let sz = (A.sizeofMutableByteArray (arr)          \
                                      `div` wordSizeInBytes) in               \
                            if (i) < 0 || (i) >= sz                           \
                              then error (BOUNDS_MSG(sz,(i)))                 \
                              else return ()
#else
#define BOUNDS_CHECK(arr,i)
#endif


------------------------------------------------------------------------------
newtype IntArray s = IA (A.MutableByteArray s)
type Elem = Word16


------------------------------------------------------------------------------
primWordToElem :: Word# -> Elem
primWordToElem w = W16# (wordToWord16# w)


------------------------------------------------------------------------------
elemToInt :: Elem -> Int
elemToInt e = let !i# = elemToInt# e
              in I# i#


------------------------------------------------------------------------------
elemToInt# :: Elem -> Int#
elemToInt# (W16# w#) = word2Int# (word16ToWord# w#)


------------------------------------------------------------------------------
elemMask :: Int
elemMask = 0xffff


------------------------------------------------------------------------------
wordSizeInBytes :: Int
wordSizeInBytes = finiteBitSize (0::Elem) `div` 8


------------------------------------------------------------------------------
-- | Cache line size, in bytes
cacheLineSize :: Int
cacheLineSize = 64


------------------------------------------------------------------------------
newArray :: PrimMonad m => Int -> m (IntArray (PrimState m))
newArray n = do
    let !sz = n * wordSizeInBytes
    !arr <- A.newAlignedPinnedByteArray sz cacheLineSize
    A.fillByteArray arr 0 sz 0
    return $! IA arr


------------------------------------------------------------------------------
readArray :: PrimMonad m => IntArray (PrimState m) -> Int -> m Elem
readArray (IA a) idx = do
    BOUNDS_CHECK(a,idx)
    A.readByteArray a idx


------------------------------------------------------------------------------
writeArray :: PrimMonad m => IntArray (PrimState m) -> Int -> Elem -> m ()
writeArray (IA a) idx val = do
    BOUNDS_CHECK(a,idx)
    A.writeByteArray a idx val


------------------------------------------------------------------------------
length :: IntArray s -> Int
length (IA a) = A.sizeofMutableByteArray a `div` wordSizeInBytes


------------------------------------------------------------------------------
toPtr :: IntArray s -> Ptr a
toPtr (IA a) = Ptr a#
  where
    !(Ptr !a#) = A.mutableByteArrayContents a

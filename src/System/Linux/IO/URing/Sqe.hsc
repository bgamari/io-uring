{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module System.Linux.IO.URing.Sqe
  ( pollAdd
  , readv
  , writev
  , sqeSize
  , SqeIndex(..)
  ) where

import Data.Word
import Data.Int
import System.Posix.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Foreign.Marshal.Utils (fillBytes)

import System.Linux.IO.URing.PollEvent
import System.Linux.IO.URing.IoVec

#include "io_uring.h"

-- | An index of the SQ entries array.
newtype SqeIndex = SqeIndex Word32
  deriving (Eq, Ord, Show, Storable, Enum)

data Sqe

-- | The user data word attached to each SQE.
type UserData = Word64

newtype SqeBuilder a = SqeBuilder (Ptr Sqe -> IO a)
  deriving (Functor)

instance Applicative SqeBuilder where
  pure x = SqeBuilder $ \_ -> return x
  SqeBuilder f <*> SqeBuilder g = SqeBuilder $ \p -> do
    h <- f p
    x <- g p
    return $! h x

instance Monad SqeBuilder where
  return = pure
  SqeBuilder f >>= g = SqeBuilder $ \p -> do
    r <- f p
    case g r of
      SqeBuilder g' -> g' p


zeroIt :: SqeBuilder ()
zeroIt = SqeBuilder $ \ptr -> fillBytes ptr 0 (#size struct io_uring_sqe)

pokeField :: (Ptr Sqe -> a -> IO ()) -> a -> SqeBuilder ()
pokeField f x = SqeBuilder $ \ptr -> f ptr x
{-# INLINE pokeField #-}

setOpCode :: Int32 -> SqeBuilder ()
setOpCode = pokeField #{poke struct io_uring_sqe, opcode}

setOff :: Word64 -> SqeBuilder ()
setOff = pokeField #{poke struct io_uring_sqe, off}

setLen :: Word32 -> SqeBuilder ()
setLen = pokeField #{poke struct io_uring_sqe, len}

setAddr :: Ptr a -> SqeBuilder ()
setAddr = pokeField #{poke struct io_uring_sqe, addr}

setFlags :: Word8 -> SqeBuilder ()
setFlags = pokeField #{poke struct io_uring_sqe, flags}

setFd :: Fd -> SqeBuilder ()
setFd fd = pokeField #{poke struct io_uring_sqe, fd} (fromIntegral fd :: Word32)

setUserData :: UserData -> SqeBuilder ()
setUserData = pokeField #{poke struct io_uring_sqe, user_data}

-- | Poll.
pollAdd
  :: Fd
  -> Event
  -> UserData
  -> SqeBuilder ()
pollAdd fd events userd = do
    zeroIt
    setOpCode (#const IORING_OP_POLL_ADD)
    setFd fd
    setUserData userd
    pokeField #{poke struct io_uring_sqe, poll_events} events

-- | Vectored read.
readv
  :: Fd          -- ^ 'Fd' to read from
  -> Word64      -- ^ offset in bytes
  -> Ptr IoVec   -- ^ IO vectors
  -> Word32      -- ^ number of IO vectors
  -> UserData
  -> SqeBuilder ()
readv fd offset iovs iov_cnt userd = do
    setOpCode (#const IORING_OP_READV)
    setFd fd
    setOff offset
    setAddr iovs
    setLen iov_cnt
    setFlags 0
    setUserData userd

-- | Vectored write.
writev
  :: Fd          -- ^ 'Fd' to read from
  -> Word64      -- ^ offset in bytes
  -> Ptr IoVec   -- ^ IO vectors
  -> Word32      -- ^ number of IO vectors
  -> UserData
  -> SqeBuilder ()
writev fd offset iovs iov_cnt userd = do
    setOpCode (#const IORING_OP_READV)
    setFd fd
    setOff offset
    setAddr iovs
    setLen iov_cnt
    setFlags 0
    setUserData userd

sqeSize :: Integral a => a
sqeSize = #{size struct io_uring_sqe}

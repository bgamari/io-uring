{-# LANGUAGE RecordWildCards #-}

module System.Linux.IO.URing.Sqe
  ( Sqe(..)
  , pokeSqe
  ) where

import Data.Word
import Data.Int
import System.Posix.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Utils (fillBytes)
import System.Linux.IO.URing.PollEvent

#include "io_uring.h"

-- | Submission Queue Entry
data Sqe
  = PollAdd { sqeFd          :: !Fd
            , sqePollEvents  :: !Event
            , sqeUserData    :: !Word64
            , sqeBufIndex    :: !Word16
            }
  | PollRemove { sqeUserData :: !Word64
               }

pokeSqe :: Ptr Sqe -> Sqe -> IO ()
pokeSqe ptr sqe = do
    zeroIt
    setUserData (sqeUserData sqe)

    case sqe of
      PollAdd {..} -> do
        setOpCode (#const IORING_OP_POLL_ADD)
        setFd sqeFd
        #{poke struct io_uring_sqe, poll_events} ptr sqePollEvents
      PollRemove {..} -> do
        setOpCode (#const IORING_OP_POLL_REMOVE)
        setFd (-1)
        #{poke struct io_uring_sqe, addr} ptr sqeUserData
  where
    zeroIt :: IO ()
    zeroIt = fillBytes ptr 0 (#size struct io_uring_sqe)

    setOpCode :: Int32 -> IO ()
    setOpCode = #{poke struct io_uring_sqe, opcode} ptr

    setFd :: Fd -> IO ()
    setFd fd = #{poke struct io_uring_sqe, fd} ptr (fromIntegral fd :: Word32)

    setUserData :: Word64 -> IO ()
    setUserData = #{poke struct io_uring_sqe, user_data} ptr

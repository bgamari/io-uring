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
import Foreign.C.Types
import Foreign.Marshal.Utils (fillBytes)

import System.Linux.IO.URing.PollEvent
import System.Linux.IO.URing.IoVec

#include "io_uring.h"


-- | Submission Queue Entry
data Sqe
  = PollAdd
      { sqeFd         :: !Fd
      , sqePollEvents :: !Event
      , sqeUserData   :: !Word64
      , sqeBufIndex   :: !Word16
      }
  | PollRemove
      { sqeUserData   :: !Word64
      }
  | Readv 
      { sqeFd         :: !Fd
      , sqeIoVecs     :: !(Ptr IoVec)
      , sqeIoVecCnt   :: !Word32
      , sqeUserData   :: !Word64
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
        -- TODO
      PollRemove {..} -> do
        setOpCode (#const IORING_OP_POLL_REMOVE)
        setFd (-1)
        -- TODO
      Readv {..} -> do
        setOpCode (#const IORING_OP_READV)
        setFd sqeFd
        setOff 0
        setAddr sqeIoVecs
        setLen sqeIoVecCnt
        setFlags 0
  where
    zeroIt :: IO ()
    zeroIt = fillBytes ptr 0 (#size struct io_uring_sqe)

    setOpCode :: Int32 -> IO ()
    setOpCode = #{poke struct io_uring_sqe, opcode} ptr

    setOff :: Word64 -> IO ()
    setOff = #{poke struct io_uring_sqe, off} ptr

    setLen :: Word32 -> IO ()
    setLen = #{poke struct io_uring_sqe, len} ptr

    setAddr :: Ptr a -> IO ()
    setAddr = #{poke struct io_uring_sqe, addr} ptr

    setFlags :: Word32 -> IO ()
    setFlags = #{poke struct io_uring_sqe, flags} ptr

    setFd :: Fd -> IO ()
    setFd fd = #{poke struct io_uring_sqe, fd} ptr (fromIntegral fd :: Word32)

    setUserData :: Word64 -> IO ()
    setUserData = #{poke struct io_uring_sqe, user_data} ptr

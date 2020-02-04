module System.Linux.IO.URing.Sqe (Sqe) where

import Data.Word
import Data.Int
import Foreign.C.Types
import System.Linux.IO.URing.PollEvent

#include "io_uring.h"

-- | Submission Queue Entry
data Sqe
  = PollAdd { sqeFd         :: !Int32
            , sqePollEvents :: !Event
            , sqeUserData   :: !Word64
            , sqeBufIndex   :: !Word16
            }
  | PollRemove { }

instance Storable Sqe where
    sizeOf _ = #size struct io_uring_sqe
    alignment _ = #alignment struct io_uring_sqe
    peek ptr =
        opcode <- #{peek struct io_uring_sqe, opcode} ptr
        case opcode of
          (#const IORING_OP_POLL_ADD) ->
                PollAdd
                    <$> #{peek struct io_uring_sqe, fd} ptr
                    <*> #{peek struct io_uring_sqe, poll_events} ptr
                    <*> #{peek struct io_uring_sqe, user_data} ptr
                    <*> #{peek struct io_uring_sqe, buf_index} ptr
            _ -> fail "unknown IORING_OP"
    poke ptr sqe =
        zeroIt
        setUserData (sqeUserData sqe)

        case sqe of
          PollAdd {..} -> do
            setOpCode (#const IORING_OP_POLL_ADD)
            setFd sqeFd
            #{poke struct io_uring_sqe, poll_events} ptr sqePollEvents
          PollRemove {..} -> do
            setOpCode (#const IORING_OP_POLL_REMOVE)
            setFd sqeFd
      where
        zeroIt :: IO ()
        zeroIt = fillBytes ptr 0 (#size struct io_uring_sqe)

        setOpCode :: Int32 -> IO ()
        setOpCode = #{poke struct io_uring_sqe, opcode} ptr

        setFd :: Fd -> IO ()
        setFd = #{poke struct io_uring_sqe, fd} ptr

        setUserData :: Word64 -> IO ()
        setUserData = #{poke struct io_uring_sqe, user_data} ptr

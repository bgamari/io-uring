{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module System.Linux.IO.URing
  ( newURing
  , postSqe
  , submit
  , popCq
    -- * Requests
    -- ** Polling
  , pollAdd
    -- ** Timeout
  , nCompletions
  , timeout
  , timeoutNCompletions
  , Timespec(..)
    -- ** Vectored I/O
  , IoVec(..)
  , readv
  , writev
    -- ** fsync
  , fsync
  , fdatasync
    -- ** Miscellaneous
  , nop
  ) where

import System.Linux.IO.URing.IoVec
import System.Linux.IO.URing.Ring
import System.Linux.IO.URing.Sqe

postSqe :: URing -> SqeBuilder a -> IO (Maybe a)
postSqe uring sqe = do
  sqeIdx_mb <- getSqe uring
  case sqeIdx_mb of
    Just sqeIdx -> do
      r <- pokeSqe sqe (sqePtr uring sqeIdx)
      pushRes <- pushSqe uring sqeIdx
      if pushRes
        then return (Just r)
        else return Nothing
    Nothing -> return Nothing


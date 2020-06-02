{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module System.Linux.IO.URing
  ( newURing
  , postSqe
  , popCq
    -- * Requests
  , nop
  , fsync
  , fdatasync
  , pollAdd
  , readv
  , writev
  , nCompletions
  , timeout
  , timeoutNCompletions
  , Timespec(..)
  ) where

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


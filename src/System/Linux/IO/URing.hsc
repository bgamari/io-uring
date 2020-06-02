{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module System.Linux.IO.URing where

import System.Posix.Types (Fd)

import System.Linux.IO.URing.PollEvent
import System.Linux.IO.URing.Cqe
import System.Linux.IO.URing.Ring
import System.Linux.IO.URing.Sqe

#include "hs_uring.h"

pollAdd :: URing -> Fd -> Event -> IO URing
pollAdd uring fd event = do
  undefined

waitCqe :: URing -> Cqe -> IO ()
waitCqe cqe = do
  undefined

postSqe :: URing -> SqeBuilder a -> IO (Maybe a)
postSqe uring sqe = do
  sqeIdx_mb <- getSqe uring
  case sqeIdx_mb of
    Just sqeIdx -> do
      r <- pokeSqe sqe (sqePtr uring sqeIdx)
      pushSqe uring sqeIdx
      return (Just r)
    Nothing -> return Nothing

popCqes :: URing -> IO [Cqe]
popCqes = undefined

readBarrier :: IO ()
readBarrier = undefined

writeBarrier :: IO ()
writeBarrier = undefined

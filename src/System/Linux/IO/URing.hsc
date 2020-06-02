{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module System.Linux.IO.URing where

import Foreign.C.Types (CInt(..), CShort(..))
import Foreign.Ptr (Ptr)
import Foreign.ForeignPtr
import Foreign.Storable (Storable(..))
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

postSqe :: URing -> Sqe -> IO (Maybe ())
postSqe uring sqe = do
  pushSQ uring $ \tl -> pokeSqe tl sqe >> return (1, ())

popCqes :: URing -> IO [Cqe]
popCqes = undefined

readBarrier :: IO ()
readBarrier = undefined

writeBarrier :: IO ()
writeBarrier = undefined

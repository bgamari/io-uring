{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module System.Linux.IO.URing where

import GHC.Base

import Foreign.C.Types (CInt(..), CShort(..))
import Foreign.Ptr (Ptr)
import Foreign.ForeignPtr
import Foreign.Storable (Storable(..))
import System.Posix.Types (Fd)

import System.Linux.IO.URing.PollEvent
import System.Linux.IO.URing.Cqe
import System.Linux.IO.URing.Ring
import System.Linux.IO.URing.Sqe


pollAdd :: URing -> Fd -> Event -> IO URing
pollAdd uring fd event = do
    hi

submit :: URing -> IO ()
submit (URing fptr) =
    throwIfNeg_ (const "io_uring_submit")
    $ withForeignPtr fptr c_io_uring_submit

waitCqe :: URing -> Cqe -> IO ()
waitCqr (URing fptr) cqe = do
    throwIfNeg_ (const "io_uring_wait_cqe")
    $ withForeignPtr fptr c_io_uring_wait_cqe

postSqe :: SRing -> Sqe -> IO ()
postSqe ring sqe = do
    writeBarrer
    hi
    writeBarrer

popCqes :: CRing -> IO [Cqe]
popCqes = undefined

readBarrier :: IO ()
readBarrier = return ()

writeBarrier :: IO ()
writeBarrier = return ()

foreign import ccall safe "liburing.h io_uring_queue_init"
    c_io_uring_queue_init :: CInt -> Ptr URing -> CInt -> IO CInt

foreign import ccall safe "liburing.h io_uring_queue_exit"
    cp_io_uring_queue_exit :: FunPtr (Ptr URing -> CNfds -> CInt -> IO CInt)

foreign import ccall safe "liburing.h io_uring_submit"
    c_io_uring_submit :: Ptr URing -> IO CInt

foreign import ccall safe "liburing.h io_uring_wait_cqe"
    c_io_uring_wait_cqe :: Ptr URing -> Ptr Cqe -> IO CInt

foreign import ccall unsafe "liburing.h io_uring_cqe_seen"
    c_io_uring_cqe_seen :: Ptr URing -> Ptr Cqe -> IO CInt


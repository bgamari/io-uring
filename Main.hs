module Main where

import Data.Bits ((.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import System.Posix.IO
import System.Posix.Files

import System.Linux.IO.URing
import System.Linux.IO.URing.PollEvent

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    uring <- newURing 128
    fd <- openFd "LICENSE" ReadOnly Nothing defaultFileFlags

    let len = 1024
    buf <- mallocArray0 len
    let iovecs = [IoVec buf (fromIntegral len)]

    putStrLn "Read some data..."
    withArrayLen iovecs $ \iovecsCnt iovecsPtr -> do
      Just (sqeIdx, _) <- postSqe uring (readv fd 0 iovecsPtr (fromIntegral iovecsCnt) 1111)
      submit uring 1 (Just 1) >>= print
      popCq uring >>= print
      freeSqe uring sqeIdx

    putStrLn "Wait 3s..."
    with (Timespec 3 0) $ \tsPtr -> do
      Just (sqeIdx, _) <- postSqe uring (timeout tsPtr 3333)
      submit uring 1 (Just 1) >>= print
      popCq uring >>= print
      freeSqe uring sqeIdx

    closeFd fd
    fd <- openFd "testing" WriteOnly (Just 0o666) defaultFileFlags

    putStrLn "Write some data..."
    withArrayLen iovecs $ \iovecsCnt iovecsPtr -> do
      Just (sqeIdx, _) <- postSqe uring (writev fd 0 iovecsPtr (fromIntegral iovecsCnt) 2222)
      n <- submit uring 1 (Just 1)
      print n
      popCq uring >>= print
      popCq uring >>= print
      freeSqe uring sqeIdx

    bufBs <- BS.unsafePackCStringLen (castPtr buf, len)
    print bufBs
    closeFd fd
    removeLink "testing"

    putStrLn "Test polling..."
    -- make a FIFO to get an fd that will block:
    createNamedPipe "fifo" (ownerReadMode .|. ownerWriteMode .|. namedPipeMode)
    let flags =  OpenFileFlags {
        append    = False,
        exclusive = False,
        noctty    = False,
        nonBlock  = True,
        trunc     = False
      }

    fd <- openFd "fifo" ReadWrite Nothing flags

    -- Test cancelation of outstanding poll:
    Just (sqeIdx, _) <- postSqe uring (pollAdd fd (pollIn) 5555)
    n <- submit uring 1 Nothing
    freeSqe uring sqeIdx
    Just (sqeIdx, _) <- postSqe uring (pollRemove 5555 6666)
    n <- submit uring 1 (Just 1)
    freeSqe uring sqeIdx
    popCq uring >>= print -- should return with a response of zero, indicating success
    popCq uring >>= print -- returns the poll with userdata 5555, also with response of 0

    -- Test cancelation of non-existing polling operation:
    Just (sqeIdx, _) <- postSqe uring (pollRemove 1234 7777) -- we never issued 1234 as userdata
    n <- submit uring 1 (Just 1)
    freeSqe uring sqeIdx
    popCq uring >>= print -- should output with a response of -2, ie ENOENT
    
    popCq uring >>= print -- should output Nothing as there are no more CQEs in the ring
    closeFd fd
    removeLink "fifo"

    putStrLn "done"

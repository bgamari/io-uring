module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import System.Posix.IO

import System.Linux.IO.URing

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
    putStrLn "done"

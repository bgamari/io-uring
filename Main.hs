module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import System.Posix.IO

import System.Linux.IO.URing
import System.Linux.IO.URing.IoVec
import System.Linux.IO.URing.Sqe
import System.Linux.IO.URing.Ring

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    uring <- newURing 128

    fd <- openFd "LICENSE" ReadOnly Nothing defaultFileFlags

    let len = 1024
    buf <- mallocArray0 len
    let iovecs = [IoVec buf (fromIntegral len)]
    withArrayLen iovecs $ \iovecsCnt iovecsPtr -> do
      postSqe uring (readv fd 0 iovecsPtr (fromIntegral iovecsCnt) 1111) >>= print
      submit uring 1 (Just 1) >>= print
      popCQ uring >>= print

    with (Timespec 0 100000000) $ \tsPtr -> do
      postSqe uring (timeout tsPtr 3333)
      submit uring 1 (Just 1) >>= print
      popCQ uring >>= print

    closeFd fd
    fd <- openFd "testing" WriteOnly (Just 0o666) defaultFileFlags

    withArrayLen iovecs $ \iovecsCnt iovecsPtr -> do
      postSqe uring (writev fd 0 iovecsPtr (fromIntegral iovecsCnt) 2222) >>= print
      n <- submit uring 1 (Just 1)
      print n
      popCQ uring >>= print
      popCQ uring >>= print

    bufBs <- BS.unsafePackCStringLen (castPtr buf, len)
    print bufBs
    closeFd fd
    putStrLn "done"

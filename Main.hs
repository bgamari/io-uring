module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

import Foreign.Marshal.Array
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
      postSqe uring $ Readv fd iovecsPtr (fromIntegral iovecsCnt) 0xdeadbeef
      n <- submit uring 1 (Just 1)
      print n

    cqe <- popCQ uring
    print cqe

    bufBs <- BS.unsafePackCStringLen (castPtr buf, len)
    print bufBs
    putStrLn "done"

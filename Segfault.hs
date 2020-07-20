{-# LANGUAGE OverloadedStrings #-}

module Main where

import Foreign.C.Types
import System.Posix.IO
import System.Posix.Types

import System.Linux.IO.URing
import System.Linux.IO.URing.PollEvent
import System.Linux.IO.URing.Cqe

import Control.Concurrent
import Control.Concurrent.MVar
import qualified Control.Exception as E
import Control.Monad (forever, void)
import qualified Data.Map as M

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

resp = "HTTP/1.0 200 OK\n\n"

main :: IO ()
main = do
  ring <- newURing 512
  ringLock <- newMVar (ring, M.empty)
  forkIO $ runTCPServer ringLock Nothing "3000" (talk ringLock)
  forever $ do
    popUntilNothing ring ringLock
    submitAndWait ring (0 :: Int) (1 :: Int)

talk :: MVar (URing,M.Map Fd (MVar ())) -> Socket -> IO ()
talk l s = do
    fd <- getFdFromSocket s
    waitForReadable l fd
    msg <- recv s 1024
    waitForWritable l fd
    sendAll s resp

-- from the "network-run" package.
runTCPServer :: MVar (URing,M.Map Fd (MVar ())) -> Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer lock mhost port server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop
  where
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock
    loop sock = forever $ do
        getFdFromSocket sock >>= waitForReadable lock
        (conn, _peer) <- accept sock
        void $ forkFinally (server conn) (const $ gracefulClose conn 5000)

waitForReadable :: MVar (URing,M.Map Fd (MVar ())) -> Fd -> IO ()
waitForReadable lock fd = waitFor pollIn lock fd
waitForWritable :: MVar (URing,M.Map Fd (MVar ())) -> Fd -> IO ()
waitForWritable lock fd = waitFor pollOut lock fd

waitFor :: Event -> MVar (URing,M.Map Fd (MVar ())) -> Fd -> IO ()
waitFor evt lock fd = do
  (ring, callbackmap) <- takeMVar lock
  mv <- newEmptyMVar
  let newMap = M.insert fd mv callbackmap
  Just (sqeIdx, _) <- postSqe ring (pollAdd fd evt (fromIntegral fd))
  n <- submit ring 1 Nothing
  freeSqe ring sqeIdx
  putMVar lock (ring, newMap)
  takeMVar mv

popUntilNothing :: URing -> MVar (URing,M.Map Fd (MVar ())) -> IO ()
popUntilNothing ring lock = do
  maybeCqe <- popCq ring
  case maybeCqe of
    Nothing -> return ()
    Just cqe -> do
      (_, callbackmap) <- readMVar lock
      -- indeed, we never delete from the map. let's see if that becomes a problem first
      case M.lookup (fromIntegral $ cqeUserData cqe) callbackmap of
        Nothing -> error "callback went missing" --- ???
        Just mv -> putMVar mv () >> popUntilNothing ring lock

getFdFromSocket :: Socket -> IO Fd
getFdFromSocket s = Fd <$> unsafeFdSocket s

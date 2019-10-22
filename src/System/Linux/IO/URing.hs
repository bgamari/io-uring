{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module URing where

import GHC.Base

import Foreign.C.Types (CInt(..), CShort(..))
import Foreign.Ptr (Ptr)
import Foreign.ForeignPtr
import Foreign.Storable (Storable(..))

import GHC.Event.PollMask

data RingType = Submission | Completion

newtype URing (a :: RingType) = URing (ForeignPtr (URing a))



type SRing = URing Submission
type CRing = URing Completion

allocURing :: Int -> IO URing
allocURing entries = do
    fptr <- mallocForeignPtrBytes (#size struct io_uring)
    addForeignPtrFinalizer cp_io_uring_queue_exit fptr
    throwIfNeg_ (const "io_uring_queue_init") $ withForeignPtr $ \ptr ->
        c_io_uring_queue_init entries ptr 0
    return (URing fptr)

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

type Fd = CInt

data Sqe' = PollAdd { sqeFd         :: !Int32
                    , sqePollEvents :: !Event
                    , sqeUserData   :: !Word64
                    , sqeBufIndex   :: !Word16
                    }

instance Storable Sqe' where
    sizeOf _ = #size struct io_uring_sqe
    alignment _ = #alignment struct io_uring_sqe
    peek ptr =
        opcode <- #{peek struct io_uring_sqe, op_code} ptr
        case opcode of
            #const IORING_OP_POLL_ADD ->
                PollAdd
                    <$> #{peek struct io_uring_sqe, fd} ptr
                    <*> #{peek struct io_uring_sqe, poll_events} ptr
                    <*> #{peek struct io_uring_sqe, user_data} ptr
                    <*> #{peek struct io_uring_sqe, buf_index} ptr
            _ -> fail "unknown IORING_OP"
    poke ptr sqe =
        zeroIt
        setUserData (sqeUserData sqe)

        case sqe of
          PollAdd {..} -> do
            setOpCode (#const IORING_OP_POLL_ADD)
            setFd sqeFd
            #{poke struct io_uring_sqe, poll_events} ptr sqePollEvents
          PollRemove {..} -> do
            setOpCode (#const IORING_OP_POLL_REMOVE)
            setFd sqeFd
      where
        zeroIt :: IO ()
        zeroIt = fillBytes ptr 0 #{size struct io_uring_sqe}

        setOpCode :: Int32 -> IO ()
        setOpCode = #{poke struct io_uring_sqe, op_code} ptr

        setFd :: Fd -> IO ()
        setFd = #{poke struct io_uring_sqe, fd} ptr

        setUserData :: Word64 -> IO ()
        setUserData = #{poke struct io_uring_sqe, user_data} ptr

postSqe :: SRing -> Sqe -> IO ()
postSqe ring sqe = do
    writeBarrer
    hi
    writeBarrer

sqRingOffset, sqRingOffset, sqesOffset :: Word64
sqRingOffset = #const IORING_OFF_SQ_RING
cqRingOffset = #const IORING_OFF_CQ_RING
sqesOffset = #const IORING_OFF_SQES

popCqes :: CRing -> IO [Cqe]
popCqes =

readBarrier :: IO ()
readBarrier = return ()

writeBarrier :: IO ()
writeBarrier = return ()

data Sqe = Sqe { sqeOpCode     :: !Word8
               , sqeFlags      :: !Word8
               , sqeIoPrio     :: !Word16
               , sqeFd         :: !Int32
               , sqeOff        :: !Word64
               , sqeAddr       :: !Word64
               , sqeLen        :: !Word32
               , sqePollEvents :: !Word16
               , sqeUserData   :: !Word64
               , sqeBufIndex   :: !Word16
               }

instance Storable Sqe where
    sizeOf _ = #size struct io_uring_sqe
    alignment _ = #alignment struct io_uring_sqe
    peek ptr =
        Sqe <$> #{peek struct io_uring_sqe, op_code} ptr
            <*> #{peek struct io_uring_sqe, flags} ptr
            <*> #{peek struct io_uring_sqe, ioprio} ptr
            <*> #{peek struct io_uring_sqe, fd} ptr
            <*> #{peek struct io_uring_sqe, off} ptr
            <*> #{peek struct io_uring_sqe, addr} ptr
            <*> #{peek struct io_uring_sqe, len} ptr
            <*> #{peek struct io_uring_sqe, poll_events} ptr
            <*> #{peek struct io_uring_sqe, user_data} ptr
            <*> #{peek struct io_uring_sqe, buf_index} ptr
    poke ptr Sqe{..} = do
        #{poke struct io_uring_sqe, op_code} ptr sqeOpCode
        #{poke struct io_uring_sqe, flags} ptr sqeFlags
        #{poke struct io_uring_sqe, ioprio} ptr sqeIoPrio
        #{poke struct io_uring_sqe, fd} ptr sqeFd
        #{poke struct io_uring_sqe, off} ptr sqeOff
        #{poke struct io_uring_sqe, addr} ptr sqeAddr
        #{poke struct io_uring_sqe, len} ptr sqeLen
        #{poke struct io_uring_sqe, poll_events} ptr sqePollEvents
        #{poke struct io_uring_sqe, user_data} ptr sqeUserData
        #{poke struct io_uring_sqe, buf_index} ptr sqeBufIndex

data Cqe = Cqe { cqeUserData   :: !Word64
               , cqeRes        :: !Int32
               , cqeFlags      :: !Int32
               }

instance Storable Cqe where
    sizeOf _ = #size struct io_uring_cqe
    alignment _ = #alignment struct io_uring_cqe
    peek ptr =
        Cqe <$> #{peek struct io_uring_cqe, user_data} ptr
            <*> #{peek struct io_uring_cqe, res} ptr
            <*> #{peek struct io_uring_cqe, flags} ptr
    poke ptr Cqe{..} = do
        #{poke struct io_uring_cqe, user_data} ptr cqeUserData
        #{poke struct io_uring_cqe, res} ptr cqeRes
        #{poke struct io_uring_cqe, flags} ptr cqeFlags

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


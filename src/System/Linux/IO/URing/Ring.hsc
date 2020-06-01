{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module System.Linux.IO.URing.Ring
  ( URing
  , newURing
  , submit
  , pushSQ
  , popCQ
  ) where

import GHC.Base
import Data.Bits
import Data.Maybe
import Data.Word

import Foreign.C.Error
import Foreign.C.Types (CInt(..), CUInt(..))
import Foreign.Ptr (Ptr, nullPtr, plusPtr, FunPtr)
import Foreign.ForeignPtr
import Foreign.Storable (Storable(..))

import System.Linux.IO.URing.Barrier
import System.Linux.IO.URing.Sqe
import System.Linux.IO.URing.Cqe

#include "io_uring.h"
#include "hs_uring.h"

data URing
  = URing { uringFptr    :: !(ForeignPtr HsURing) 
          , uringFd      :: !CInt
          , uringSQ      :: !SubmissionQueue
          , uringCQ      :: !CompletionQueue
          }

data SubmissionQueue
  = SQ { uringSQHead     :: !(Ptr Word32)
       , uringSQTail     :: !(Ptr Word32)
       , uringSQRingMask :: !(Ptr Word32)
       , uringSQEArray   :: !(Ptr Sqe)
       }

data CompletionQueue
  = CQ { uringCQHead     :: !(Ptr Word32)
       , uringCQTail     :: !(Ptr Word32)
       , uringCQRingMask :: !(Ptr Word32)
       , uringCQArray    :: !(Ptr Cqe)
       }

newURing :: Int -> IO URing
newURing entries = do
    u <- c_new_uring (fromIntegral entries)
    when (u == nullPtr)
      $ fail "Failed to create uring"
    fptr <- newForeignPtr c_free_uring u

    HsURing {..} <- peekHsURing u
    let uringSQ =
          SQ
            { uringSQHead     = sqPtr sqroHead
            , uringSQTail     = sqPtr sqroTail
            , uringSQRingMask = sqPtr sqroRingMask
            , uringSQEArray   = sqPtr sqroRingEntries
            }
          where sqPtr field = sqAperture `plusPtr` fromIntegral (field $ uringpSQRingOffsets params)
          
        uringCQ =
          CQ
            { uringCQHead     = cqPtr cqroHead
            , uringCQTail     = cqPtr cqroTail
            , uringCQRingMask = cqPtr cqroRingMask
            , uringCQArray    = cqPtr cqroCqes
            }
          where cqPtr field = cqAperture `plusPtr` fromIntegral (field $ uringpCQRingOffsets params)

    let dbg lbl x = putStrLn $ lbl ++ ": " ++ show x
    dbg "sqe array" $ uringSQEArray uringSQ
    dbg "sqe tail"  $ uringSQTail uringSQ

    return $ URing { uringFptr = fptr
                   , uringFd = hsURingFd
                   , ..
                   }

submit :: URing
       -> Int       -- ^ number to submit
       -> Maybe Int -- ^ minimum to complete
       -> IO Int
submit uring to_submit min_complete = do
    let flags = if isJust min_complete then #{const IORING_ENTER_GETEVENTS} else 0
    fmap fromIntegral $ throwErrnoIfMinus1 "io_uring_enter" $ c_io_uring_enter
      (uringFd uring)
      (fromIntegral to_submit)
      (maybe 0 fromIntegral min_complete)
      flags
      nullPtr

pushSQ :: URing -> (Ptr Sqe -> IO (Int, a)) -> IO a
pushSQ uring push = do
    tail0 <- peek (uringSQTail $ uringSQ uring)
    readBarrier
    mask <- peek (uringSQRingMask $ uringSQ uring)
    let index = tail0 .&. mask
    let tail_ptr = uringSQEArray (uringSQ uring) `plusPtr` fromIntegral index
    print ("Push", tail_ptr)
    (n_pushed, r) <- push tail_ptr
    let tail' = tail0 + fromIntegral n_pushed
    writeBarrier
    poke (uringSQTail $ uringSQ uring) tail'
    return r

popCQ :: URing -> IO (Maybe Cqe)
popCQ uring = do
    hd <- peek $ uringCQHead $ uringCQ uring
    tl <- peek $ uringCQTail $ uringCQ uring
    if hd == tl
      then return Nothing
      else do
        readBarrier
        mask <- peek (uringCQRingMask $ uringCQ uring)
        let index = hd .&. mask
        cqe <- peek (uringCQArray (uringCQ uring) `plusPtr` fromIntegral index)
        poke (uringCQHead $ uringCQ uring) (succ hd)
        return (Just cqe)


-----------------------------------------------------------
-- The C interface
-----------------------------------------------------------

data IOURingParams
  = IOURingParams { uringpSQEntries     :: !Word32
                  , uringpCQEntries     :: !Word32
                  , uringpFlags         :: !Word32
                  , uringpSQThreadCpu   :: !Word32
                  , uringpSQThreadIdle  :: !Word32
                  , uringpSQRingOffsets :: !SQRingOffsets
                  , uringpCQRingOffsets :: !CQRingOffsets
                  }

peekIOURingParams :: Ptr IOURingParams -> IO IOURingParams
peekIOURingParams p =
  IOURingParams
    <$> #{peek struct io_uring_params, sq_entries} p
    <*> #{peek struct io_uring_params, cq_entries} p
    <*> #{peek struct io_uring_params, flags} p
    <*> #{peek struct io_uring_params, sq_thread_cpu} p
    <*> #{peek struct io_uring_params, sq_thread_idle} p
    <*> peekSQRingOffsets (#{ptr struct io_uring_params, sq_off} p)
    <*> peekCQRingOffsets (#{ptr struct io_uring_params, cq_off} p)

-- | @struct io_cqring_offsets@.
data CQRingOffsets
  = CQRingOffsets { cqroHead        :: !Word32 
                  , cqroTail        :: !Word32
                  , cqroRingMask    :: !Word32
                  , cqroRingEntries :: !Word32
                  , cqroOverflow    :: !Word32
                  , cqroCqes        :: !Word32
                  }

peekCQRingOffsets :: Ptr CQRingOffsets -> IO CQRingOffsets
peekCQRingOffsets p =
  CQRingOffsets
    <$> #{peek struct io_cqring_offsets, head}         p
    <*> #{peek struct io_cqring_offsets, tail}         p
    <*> #{peek struct io_cqring_offsets, ring_mask}    p
    <*> #{peek struct io_cqring_offsets, ring_entries} p
    <*> #{peek struct io_cqring_offsets, overflow}     p
    <*> #{peek struct io_cqring_offsets, cqes}         p

-- | @struct io_sqring_offsets@.
data SQRingOffsets
  = SQRingOffsets { sqroHead        :: !Word32 
                  , sqroTail        :: !Word32
                  , sqroRingMask    :: !Word32
                  , sqroRingEntries :: !Word32
                  , sqroFlags       :: !Word32
                  , sqroDropped     :: !Word32
                  , sqroArray       :: !Word32
                  }

peekSQRingOffsets :: Ptr SQRingOffsets -> IO SQRingOffsets
peekSQRingOffsets p =
  SQRingOffsets
    <$> #{peek struct io_sqring_offsets, head}         p
    <*> #{peek struct io_sqring_offsets, tail}         p
    <*> #{peek struct io_sqring_offsets, ring_mask}    p
    <*> #{peek struct io_sqring_offsets, ring_entries} p
    <*> #{peek struct io_sqring_offsets, flags}        p
    <*> #{peek struct io_sqring_offsets, dropped}      p
    <*> #{peek struct io_sqring_offsets, array}        p

data HsURing
    = HsURing { sqeAperture :: !(Ptr ())
              , sqAperture  :: !(Ptr ())
              , cqAperture  :: !(Ptr ())
              , hsURingFd     :: !CInt
              , params      :: !IOURingParams
              }

peekHsURing :: Ptr HsURing -> IO HsURing
peekHsURing p =
  HsURing
    <$> #{peek struct hs_uring, sqe_aperture} p
    <*> #{peek struct hs_uring, sq_aperture}  p
    <*> #{peek struct hs_uring, cq_aperture}  p
    <*> #{peek struct hs_uring, uring_fd}     p
    <*> peekIOURingParams (#{ptr struct hs_uring, params} p)

foreign import ccall "hs_new_uring"
    c_new_uring :: CInt -> IO (Ptr HsURing)
foreign import ccall "&hs_free_uring"
    c_free_uring :: FunPtr (Ptr HsURing -> IO ())
foreign import ccall "io_uring_enter"
    c_io_uring_enter :: CInt    -- ^ fd
                     -> CUInt   -- ^ to_submit
                     -> CUInt   -- ^ min_complete
                     -> CUInt   -- ^ flags
                     -> Ptr a   -- ^ sig
                     -> IO CInt


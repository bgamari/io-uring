{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module System.Linux.IO.URing.Ring where

import GHC.Base
import Data.Word

import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr, nullPtr, plusPtr, FunPtr)
import Foreign.ForeignPtr
import Foreign.Storable (Storable(..))

#include "io_uring.h"
#include "hs_uring.h"

data URing = URing { uringFptr :: !(ForeignPtr HsURing) 
                   , uringSubmissionQueue :: !SubmissionQueue
                   , uringCompletionQueue :: !CompletionQueue
                   }

data SubmissionQueue
  = SQ { uringSQHead   :: !(Ptr Word32)
       , uringSQTail   :: !(Ptr Word32)
       , uringSQEArray :: !(Ptr Word32)
       }

data CompletionQueue
  = CQ { uringCQHead  :: !(Ptr Word32)
       , uringCQTail  :: !(Ptr Word32)
       , uringCQArray :: !(Ptr Word32)
       }

newURing :: Int -> IO URing
newURing entries = do
    u <- c_new_uring (fromIntegral entries)
    when (u == nullPtr)
      $ fail "Failed to create uring"
    fptr <- newForeignPtr c_free_uring u

    HsURing {..} <- peek u
    let uringSubmissionQueue =
          SubmissionQueue
            { uringSQHead = sqAperture `plusPtr` sqroHead params 
            , uringSQTail = sqAperture `plusPtr` sqroTail params 
            , uringSQRingMask = sqAperture `plusPtr` sqroRingMask params 
            }
        uringCompletionQueue =
          CompletionQueue
            { uringCQHead = sqAperture `plusPtr` cqroHead params
            , uringCQTail = sqAperture `plusPtr` cqroTail params
            , uringCQTail = sqAperture `plusPtr` cqroTail params
            }

    return $ URing { uringFptr = fptr
                   , ..
                   }

sqRingOffset, cqRingOffset, sqesOffset :: Word64
sqRingOffset = #const IORING_OFF_SQ_RING
cqRingOffset = #const IORING_OFF_CQ_RING
sqesOffset   = #const IORING_OFF_SQES

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
    <$> #{peek struct io_cqring_offsets, head} p
    <*> #{peek struct io_cqring_offsets, tail} p
    <*> #{peek struct io_cqring_offsets, ring_mask} p
    <*> #{peek struct io_cqring_offsets, ring_entries} p
    <*> #{peek struct io_cqring_offsets, overflow} p
    <*> #{peek struct io_cqring_offsets, cqes} p

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
    <$> #{peek struct io_sqring_offsets, head} p
    <*> #{peek struct io_sqring_offsets, tail} p
    <*> #{peek struct io_sqring_offsets, ring_mask} p
    <*> #{peek struct io_sqring_offsets, ring_entries} p
    <*> #{peek struct io_sqring_offsets, flags} p
    <*> #{peek struct io_sqring_offsets, dropped} p
    <*> #{peek struct io_sqring_offsets, array} p

mkURing :: Int -> IO URing
mkURing entries = do
  return undefined

data HsURing
    = HsURing { sqeAperture :: !(Ptr ())
              , sqAperture  :: !(Ptr ())
              , cqAperture  :: !(Ptr ())
              , params      :: !IOURingParams
              }

peekHsURing :: Ptr HsURing -> IO HsURing
peekHsURing p =
  HsURing
    <$> #{peek struct hs_uring, sqe_aperture} p
    <*> #{peek struct hs_uring, sq_aperture} p
    <*> #{peek struct hs_uring, cq_aperture} p
    <*> #{peek struct hs_uring, params} p

foreign import ccall "hs_new_uring"
    c_new_uring :: CInt -> IO (Ptr HsURing)
foreign import ccall "&hs_free_uring"
    c_free_uring :: FunPtr (Ptr HsURing -> IO ())

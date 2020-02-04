{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module System.Linux.IO.URing.Ring where

import GHC.Base
import Data.Word

import Foreign.C.Types (CInt(..), CShort(..))
import Foreign.Ptr (Ptr)
import Foreign.ForeignPtr
import Foreign.Storable (Storable(..))

#include "io_uring.h"
#include "hs_uring.h"

data RingType = Submission | Completion

data URing (a :: RingType) = URing !(ForeignPtr (URing a)) !Fd

type SRing = URing Submission
type CRing = URing Completion

allocURing :: Int -> IO (URing a)
allocURing entries = do
    fptr <- mallocForeignPtrBytes (#size struct hs_sq_ring)
    addForeignPtrFinalizer cp_io_uring_queue_exit fptr
    throwIfNeg_ (const "io_uring_queue_init") $ withForeignPtr $ \ptr ->
        c_io_uring_queue_init entries ptr 0
    return (URing fptr)

sqRingOffset, cqRingOffset, sqesOffset :: Word64
sqRingOffset = #const IORING_OFF_SQ_RING
cqRingOffset = #const IORING_OFF_CQ_RING
sqesOffset   = #const IORING_OFF_SQES

data IOURingParams
  = IOURingParams { sq_entries :: Word32}

foreign ccall safe "io_uring_setup"
  c_io_uring_setup :: CUInt -> Ptr IOURingParams

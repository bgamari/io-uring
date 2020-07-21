{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE UnboxedTuples #-}

#define WITH_BARRIERS

-- | Memory barriers.
module System.Linux.IO.URing.Barrier (readBarrier, writeBarrier) where

import GHC.IO
import GHC.Exts

-- TODO: Ideally these would be primops
foreign import prim "uring_write_barrier"
    hs_writeBarrier :: State# RealWorld -> (# State# RealWorld #)
foreign import prim "uring_read_barrier"
    hs_readBarrier :: State# RealWorld -> (# State# RealWorld #)

readBarrier :: IO ()
readBarrier =
#if defined(WITH_BARRIERS)
  IO $ \s ->
    case hs_readBarrier s of
      (# s' #) -> (# s', () #)
#else
  return ()
#endif

writeBarrier :: IO ()
writeBarrier =
#if defined(WITH_BARRIERS)
  IO $ \s ->
    case hs_writeBarrier s of
      (# s' #) -> (# s', () #)
#else
  return ()
#endif

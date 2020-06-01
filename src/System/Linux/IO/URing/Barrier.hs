{-# LANGUAGE MagicHash #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Memory barriers.
module System.Linux.IO.URing.Barrier (readBarrier, writeBarrier) where

import GHC.IO
import GHC.Exts

-- TODO: Ideally these would be primops
foreign import prim "uring_write_barrier"
    hs_writeBarrier :: State# RealWorld -> (# State# RealWorld, () #)
foreign import prim "uring_read_barrier"
    hs_readBarrier :: State# RealWorld -> (# State# RealWorld, () #)

readBarrier :: IO ()
readBarrier = IO $ \s ->
  case hs_readBarrier s of
    (# s', _ #) -> (# s', () #)

writeBarrier :: IO ()
writeBarrier = IO $ \s ->
  case hs_writeBarrier s of
    (# s', _ #) -> (# s', () #)

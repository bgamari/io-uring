{-# LANGUAGE RecordWildCards #-}

module System.Linux.IO.URing.Cqe (Cqe(..))  where

import Data.Word
import Data.Int
import Foreign.Storable

#include "io_uring.h"

-- | Completion Queue Entry
data Cqe
  = Cqe { cqeUserData   :: !Word64
        , cqeRes        :: !Int32
        , cqeFlags      :: !Int32
        }
  deriving (Show)

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

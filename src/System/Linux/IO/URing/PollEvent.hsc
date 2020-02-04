{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE GeneralizedNewtypeDeriving
           , NoImplicitPrelude
           , BangPatterns
  #-}

module System.Linux.IO.URing.PollEvent
    (
#if defined(HAVE_POLL_H)
      Event(..), fromEvent, toEvent
#endif
    ) where

#if defined(HAVE_POLL_H)

#include <poll.h>

import Data.Bits (Bits, FiniteBits, (.|.), (.&.))
import Foreign.C.Types (CShort(..))
import Foreign.Storable (Storable(..))
import GHC.Base
import qualified GHC.Event.Internal as E

-- | A @poll@ event mask.
newtype Event = Event CShort
    deriving ( Eq         -- ^ @since 4.4.0.0
             , Show       -- ^ @since 4.4.0.0
             , Num        -- ^ @since 4.4.0.0
             , Storable   -- ^ @since 4.4.0.0
             , Bits       -- ^ @since 4.4.0.0
             , FiniteBits -- ^ @since 4.7.0.0
             )

#{enum Event, Event
 , pollIn    = POLLIN
 , pollOut   = POLLOUT
 , pollErr   = POLLERR
 , pollHup   = POLLHUP
 }

fromEvent :: E.Event -> Event
fromEvent e = remap E.evtRead  pollIn .|.
              remap E.evtWrite pollOut
  where remap evt to
            | e `E.eventIs` evt = to
            | otherwise         = 0

toEvent :: Event -> E.Event
toEvent e = remap (pollIn .|. pollErr .|. pollHup)  E.evtRead `mappend`
            remap (pollOut .|. pollErr .|. pollHup) E.evtWrite
  where remap evt to
            | e .&. evt /= 0 = to
            | otherwise      = mempty

#endif /* defined(HAVE_POLL_H) */

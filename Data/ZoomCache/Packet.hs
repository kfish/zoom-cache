{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      : Data.ZoomCache.Write
-- Copyright   : Conrad Parker
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Conrad Parker <conrad@metadecks.org>
-- Stability   : unstable
-- Portability : unknown
--
-- ZoomCache packet definition
----------------------------------------------------------------------

module Data.ZoomCache.Packet (
    -- * Types
      Packet(..)
) where

import Data.ZoomCache.Common

------------------------------------------------------------

data Packet a = Packet
    { packetTrack      :: TrackNo
    , packetEntryTime  :: TimeStamp
    , packetExitTime   :: TimeStamp
    , packetCount      :: Int
    , packetData       :: a
    , packetTimeStamps :: [TimeStamp]
    }

------------------------------------------------------------

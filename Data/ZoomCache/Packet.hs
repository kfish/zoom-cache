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
    , PacketData(..)
) where

import Data.ZoomCache.Common

------------------------------------------------------------

data PacketData = PDDouble [Double] | PDInt [Int]

data Packet = Packet
    { packetTrack      :: TrackNo
    , packetEntryTime  :: TimeStamp
    , packetExitTime   :: TimeStamp
    , packetCount      :: Int
    , packetData       :: PacketData
    , packetTimeStamps :: [TimeStamp]
    }

------------------------------------------------------------

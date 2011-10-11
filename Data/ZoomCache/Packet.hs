{-# OPTIONS -Wall #-}

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

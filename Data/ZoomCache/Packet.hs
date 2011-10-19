{-# LANGUAGE TypeFamilies #-}
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
    -- * Classes
      ZoomRead(..)
    , PacketData(..)

    -- * Types
    , Packet(..)
) where

import Data.Dynamic

import Data.ZoomCache.Common

------------------------------------------------------------

class ZoomRead a where
    data PacketData a :: *

instance ZoomRead Dynamic where
    data PacketData Dynamic = PDDynamic [Dynamic]

instance ZoomRead Double where
    data PacketData Double = PDDouble [Double]

instance ZoomRead Int where
    data PacketData Int = PDInt [Int]

------------------------------------------------------------

data Packet a = Packet
    { packetTrack      :: TrackNo
    , packetEntryTime  :: TimeStamp
    , packetExitTime   :: TimeStamp
    , packetCount      :: Int
    , packetData       :: PacketData a
    , packetTimeStamps :: [TimeStamp]
    }

------------------------------------------------------------

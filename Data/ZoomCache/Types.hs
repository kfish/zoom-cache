{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      : Data.ZoomCache.Types
-- Copyright   : Conrad Parker
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Conrad Parker <conrad@metadecks.org>
-- Stability   : unstable
-- Portability : unknown
--
-- ZoomCache packet and summary types and interfaces
----------------------------------------------------------------------

module Data.ZoomCache.Types (
    -- * Classes
      ZoomReadable(..)
    , RawData()
    , ZoomWritable(..)

    , ZoomRaw(..)

    , ZoomSummary(..)

    , ZoomWork(..)

    -- * Types
    , Packet(..)
    , Summary(..)
    , SummaryData()

    , summaryDuration
) where

import Blaze.ByteString.Builder
import Control.Monad.Trans (MonadIO)
import Data.Dynamic
import Data.IntMap (IntMap)
import Data.Iteratee (Iteratee)
import Data.Word

import Data.ZoomCache.Common

------------------------------------------------------------

data Packet = Packet
    { packetTrack      :: TrackNo
    , packetEntryTime  :: TimeStamp
    , packetExitTime   :: TimeStamp
    , packetCount      :: Int
    , packetData       :: ZoomRaw
    , packetTimeStamps :: [TimeStamp]
    }

------------------------------------------------------------
-- | A recorded block of summary data
data Summary a = Summary
    { summaryTrack :: TrackNo
    , summaryLevel :: Int
    , summaryEntryTime :: TimeStamp
    , summaryExitTime :: TimeStamp
    , summaryData :: SummaryData a
    }

-- | The duration covered by a summary, in units of 1 / the track's datarate
summaryDuration :: Summary a -> Integer
summaryDuration s = (unTS $ summaryExitTime s) - (unTS $ summaryEntryTime s)

------------------------------------------------------------
-- Read

class ZoomReadable a where
    data RawData a  :: *
    readRaw         :: (Functor m, MonadIO m)
                    => Iteratee [Word8] m a
    fromList        :: [a] -> RawData a

    data SummaryData a :: *
    readSummary        :: (Functor m, MonadIO m)
                       => Iteratee [Word8] m (SummaryData a)

    prettyRawData      :: RawData a -> [String]
    prettySummaryData  :: SummaryData a -> String
    -- typeOfSummaryData :: SummaryData a -> TypeRep

data ZoomRaw = forall a . ZoomReadable a => ZoomRaw (RawData a)

data ZoomSummary = forall a . ZoomReadable a => ZoomSummary (Summary a)

------------------------------------------------------------
-- Write

class ZoomWritable a where
    data SummaryWork a :: *

    fromRaw            :: a -> Builder
    fromSummaryData    :: SummaryData a -> Builder

    initSummaryWork    :: TimeStamp -> SummaryWork a
    toSummaryData      :: Double -> SummaryWork a -> SummaryData a
    updateSummaryData  :: Int -> TimeStamp -> a
                       -> SummaryWork a
                       -> SummaryWork a
    appendSummaryData  :: Double -> SummaryData a
                       -> Double -> SummaryData a
                       -> SummaryData a

data ZoomWork = forall a . (Typeable a, ZoomWritable a) => ZoomWork
    { levels   :: IntMap (Summary a -> Summary a)
    , currWork :: Maybe (SummaryWork a)
    }

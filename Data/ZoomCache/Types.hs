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
import qualified Data.ByteString.Lazy as L
import Data.Dynamic
import Data.Int
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
summaryDuration :: Summary a -> Int64
summaryDuration s = (unTS $ summaryExitTime s) - (unTS $ summaryEntryTime s)

------------------------------------------------------------
-- Read

-- | A codec instance must specify a 'SummaryData' type,
-- and implement all methods of this class.
class ZoomReadable a where
    -- | Summaries of a subsequence of values of type 'a'. In the default
    -- instances for 'Int' and 'Double', this is a record containing values
    -- such as the maximum, minimum and mean of the subsequence.
    data SummaryData a :: *

    -- | The track identifier used for streams of type 'a'.
    -- The /value/ of the argument should be ignored by any instance of
    -- 'ZoomReadable', so that is safe to pass 'undefined' as the
    -- argument.
    trackIdentifier :: a -> L.ByteString

    -- | An iteratee to read one value of type 'a' from a stream of '[Word8]'.
    readRaw         :: (Functor m, MonadIO m)
                    => Iteratee [Word8] m a

    -- | An iteratee to read one value of type 'SummaryData a' from a stream
    -- of '[Word8]'.
    readSummary        :: (Functor m, MonadIO m)
                       => Iteratee [Word8] m (SummaryData a)

    -- | Pretty printing, used for dumping values of type 'a'.
    prettyRaw          :: a -> String

    -- | Pretty printing for values of type 'SummaryData a'.
    prettySummaryData  :: SummaryData a -> String
    -- typeOfSummaryData :: SummaryData a -> TypeRep

data ZoomRaw = forall a . ZoomReadable a => ZoomRaw [a]

data ZoomSummary = forall a . ZoomReadable a => ZoomSummary (Summary a)

------------------------------------------------------------
-- Write

-- | A codec instance must additionally specify a 'SummaryWork' type
class ZoomReadable a => ZoomWritable a where
    -- | Intermediate calculations
    data SummaryWork a :: *

    -- | Serialize a value of type 'a'
    fromRaw            :: a -> Builder

    -- | Serialize a 'SummaryData a'
    fromSummaryData    :: SummaryData a -> Builder

    -- | Generate a new 'SummaryWork a', given an initial timestamp.
    initSummaryWork    :: TimeStamp -> SummaryWork a

    -- | Update a 'SummaryData' with the value of 'a' occuring at the
    -- given 'TimeStamp'.
    updateSummaryData  :: Int -> TimeStamp -> a
                       -> SummaryWork a
                       -> SummaryWork a

    -- | Finalize a 'SummaryWork a', generating a 'SummaryData a'.
    toSummaryData      :: Double -> SummaryWork a -> SummaryData a

    -- | Append two 'SummaryData'
    appendSummaryData  :: Double -> SummaryData a
                       -> Double -> SummaryData a
                       -> SummaryData a

data ZoomWork = forall a . (Typeable a, ZoomWritable a) => ZoomWork
    { levels   :: IntMap (Summary a -> Summary a)
    , currWork :: Maybe (SummaryWork a)
    }

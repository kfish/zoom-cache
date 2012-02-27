{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
    -- * Track types and specification
      Codec(..)
    , TrackMap
    , TrackSpec(..)
    , IdentifyCodec

    -- * Classes
    , Timestampable(..)
    , before

    , UTCTimestampable(..)
    , beforeUTC

    , ZoomReadable(..)
    , ZoomWritable(..)

    , ZoomRaw(..)

    , ZoomSummary(..)
    , ZoomSummaryUTC(..)
    , ZoomSummarySO(..)

    , ZoomWork(..)

    -- * Types
    , Packet(..)
    , packetFromPacketSO
    , Summary(..)
    , summaryFromSummarySO

    , PacketUTC(..)
    , packetUTCFromPacket
    , packetUTCFromPacketSO

    , SummaryUTC(..)
    , summaryUTCFromSummary
    , summaryUTCFromSummarySO

    , PacketSO(..)
    , SummarySO(..)

    , summarySODuration

    -- * CacheFile
    , CacheFile(..)
    , mkCacheFile
    , fiFull

) where

import Blaze.ByteString.Builder
import Data.ByteString (ByteString)
import Data.Dynamic
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Iteratee (Iteratee)
import Data.Maybe (fromJust)
import Data.Time (UTCTime)
import System.Posix.Types (FileOffset)

import Data.Offset
import Data.ZoomCache.Common

------------------------------------------------------------
-- | A map of all track numbers to their 'TrackSpec'
type TrackMap = IntMap TrackSpec

-- | A specification of the type and name of each track
data TrackSpec = TrackSpec
    { specType         :: !Codec
    , specDeltaEncode  :: !Bool
    , specZlibCompress :: !Bool
    , specSRType       :: !SampleRateType
    , specRate         :: {-# UNPACK #-}!Rational
    , specName         :: !ByteString
    }
    deriving (Show)

data Codec = forall a . ZoomReadable a => Codec a

instance Show Codec where
    show = const "<<Codec>>"

-- | Identify the tracktype corresponding to a given Codec Identifier.
-- When parsing a zoom-cache file, the zoom-cache library will try each
-- of a given list ['IdentifyTrack'].
--
-- The standard zoom-cache instances are provided in 'standardIdentifiers'.
--
-- When developing your own codecs it is not necessary to build a composite
-- 'IdentifyTrack' functions; it is sufficient to generate one for each new
-- codec type. A library of related zoom-cache codecs should export its own
-- ['IdentifyTrack'] functions, usually called something like mylibIdentifiers.
--
-- These can be generated with 'identifyCodec'.
type IdentifyCodec = ByteString -> Maybe Codec

------------------------------------------------------------

-- | Global and track headers for a zoom-cache file
data CacheFile = CacheFile
    { cfGlobal :: Global
    , cfSpecs  :: IntMap TrackSpec
    , cfOffsets :: IntMap FileOffset
    }

-- | Create an empty 'CacheFile' using the given 'Global'
mkCacheFile :: Global -> CacheFile
mkCacheFile g = CacheFile g IM.empty IM.empty

-- | Determine whether all tracks of a 'CacheFile' are specified
fiFull :: CacheFile -> Bool
fiFull CacheFile{..} = IM.size cfSpecs == noTracks cfGlobal

------------------------------------------------------------

class Timestampable a where
    timestamp :: a -> Maybe TimeStamp

before :: (Timestampable a) => Maybe TimeStamp -> a -> Bool
before Nothing _ = True
before (Just b) x = t == Nothing || (fromJust t) < b
  where
    t = timestamp x

instance Timestampable (TimeStamp, a) where
    timestamp = Just . fst

instance Timestampable a => Timestampable [a] where
    timestamp []    = Nothing
    timestamp (x:_) = timestamp x

instance Timestampable a => Timestampable (Offset a) where
    timestamp (Offset _ xs) = timestamp xs

------------------------------------------------------------

class UTCTimestampable a where
    utcTimestamp :: a -> Maybe UTCTime

beforeUTC :: (UTCTimestampable a) => Maybe UTCTime -> a -> Bool
beforeUTC Nothing _ = True
beforeUTC (Just b) x = t == Nothing || (fromJust t) < b
  where
    t = utcTimestamp x

instance UTCTimestampable (UTCTime, a) where
    utcTimestamp = Just . fst

instance UTCTimestampable a => UTCTimestampable [a] where
    utcTimestamp []    = Nothing
    utcTimestamp (x:_) = utcTimestamp x

instance UTCTimestampable a => UTCTimestampable (Offset a) where
    utcTimestamp (Offset _ xs) = utcTimestamp xs

------------------------------------------------------------

data PacketSO = PacketSO
    { packetSOTrack         :: {-# UNPACK #-}!TrackNo
    , packetSOEntry         :: {-# UNPACK #-}!SampleOffset
    , packetSOExit          :: {-# UNPACK #-}!SampleOffset
    , packetSOCount         :: {-# UNPACK #-}!Int
    , packetSOData          :: !ZoomRaw
    , packetSOSampleOffsets :: ![SampleOffset]
    }

data Packet = Packet
    { packetTrack      :: {-# UNPACK #-}!TrackNo
    , packetEntry      :: {-# UNPACK #-}!TimeStamp
    , packetExit       :: {-# UNPACK #-}!TimeStamp
    , packetCount      :: {-# UNPACK #-}!Int
    , packetData       :: !ZoomRaw
    , packetTimeStamps :: ![TimeStamp]
    }

packetFromPacketSO :: Rational -> PacketSO -> Packet
packetFromPacketSO r PacketSO{..} = Packet {
      packetTrack = packetSOTrack
    , packetEntry = timeStampFromSO r packetSOEntry
    , packetExit  = timeStampFromSO r packetSOExit
    , packetCount = packetSOCount
    , packetData  = packetSOData
    , packetTimeStamps = map (timeStampFromSO r) packetSOSampleOffsets
    }

instance Timestampable Packet where
    timestamp = Just . packetEntry

data PacketUTC = PacketUTC
    { packetUTCTrack      :: {-# UNPACK #-}!TrackNo
    , packetUTCEntry      :: {-# UNPACK #-}!UTCTime
    , packetUTCExit       :: {-# UNPACK #-}!UTCTime
    , packetUTCCount      :: {-# UNPACK #-}!Int
    , packetUTCData       :: !ZoomRaw
    , packetUTCTimeStamps :: ![UTCTime]
    }

packetUTCFromPacket :: UTCTime -> Packet -> PacketUTC
packetUTCFromPacket base Packet{..} = PacketUTC {
      packetUTCTrack = packetTrack
    , packetUTCEntry = utcTimeFromTimeStamp base packetEntry
    , packetUTCExit  = utcTimeFromTimeStamp base packetExit
    , packetUTCCount = packetCount
    , packetUTCData  = packetData
    , packetUTCTimeStamps = map (utcTimeFromTimeStamp base) packetTimeStamps
    }

packetUTCFromPacketSO :: UTCTime -> Rational -> PacketSO -> PacketUTC
packetUTCFromPacketSO base r = packetUTCFromPacket base . packetFromPacketSO r

instance UTCTimestampable PacketUTC where
    utcTimestamp = Just . packetUTCEntry

------------------------------------------------------------
-- | A recorded block of summary data
data SummarySO a = SummarySO
    { summarySOTrack :: {-# UNPACK #-}!TrackNo
    , summarySOLevel :: {-# UNPACK #-}!Int
    , summarySOEntry :: {-# UNPACK #-}!SampleOffset
    , summarySOExit  :: {-# UNPACK #-}!SampleOffset
    , summarySOData  :: !(SummaryData a)
    }
    deriving (Typeable)

-- | The duration covered by a summary, in units of 1 / the track's datarate
summarySODuration :: SummarySO a -> SampleOffsetDiff
summarySODuration s = SODiff $ (unSO $ summarySOExit s) - (unSO $ summarySOEntry s)


-- | A summary block with samplecounts converted to TimeStamp
data Summary a = Summary
    { summaryTrack :: {-# UNPACK #-}!TrackNo
    , summaryLevel :: {-# UNPACK #-}!Int
    , summaryEntry :: {-# UNPACK #-}!TimeStamp
    , summaryExit  :: {-# UNPACK #-}!TimeStamp
    , summaryData  :: !(SummaryData a)
    }
    deriving (Typeable)

-- | Convert a SummarySo to a Summary, given a samplerate
summaryFromSummarySO :: Rational -> SummarySO a -> Summary a
summaryFromSummarySO r SummarySO{..} = Summary {
      summaryTrack = summarySOTrack
    , summaryLevel = summarySOLevel
    , summaryEntry = timeStampFromSO r summarySOEntry
    , summaryExit  = timeStampFromSO r summarySOExit
    , summaryData  = summarySOData
    } 

instance Timestampable (Summary a) where
    timestamp = Just . summaryEntry

-- | A summary block with timestamps converted to UTCTime
data SummaryUTC a = SummaryUTC
    { summaryUTCTrack :: {-# UNPACK #-}!TrackNo
    , summaryUTCLevel :: {-# UNPACK #-}!Int
    , summaryUTCEntry :: {-# UNPACK #-}!UTCTime
    , summaryUTCExit  :: {-# UNPACK #-}!UTCTime
    , summaryUTCData  :: !(SummaryData a)
    }
    deriving (Typeable)

-- | Convert a Summary to a SummaryUTC, given a UTC base time
summaryUTCFromSummary :: UTCTime -> Summary a -> SummaryUTC a
summaryUTCFromSummary base Summary{..} = SummaryUTC {
      summaryUTCTrack = summaryTrack
    , summaryUTCLevel = summaryLevel
    , summaryUTCEntry = utcTimeFromTimeStamp base summaryEntry
    , summaryUTCExit  = utcTimeFromTimeStamp base summaryExit
    , summaryUTCData  = summaryData
    }

summaryUTCFromSummarySO :: UTCTime -> Rational -> SummarySO a -> SummaryUTC a
summaryUTCFromSummarySO base r = summaryUTCFromSummary base . summaryFromSummarySO r

instance UTCTimestampable (SummaryUTC a) where
    utcTimestamp = Just . summaryUTCEntry

------------------------------------------------------------
-- Read

-- | A codec instance must specify a 'SummaryData' type,
-- and implement all methods of this class.
class Typeable a => ZoomReadable a where
    -- | Summaries of a subsequence of values of type 'a'. In the default
    -- instances for 'Int' and 'Double', this is a record containing values
    -- such as the maximum, minimum and mean of the subsequence.
    data SummaryData a :: *

    -- | The track identifier used for streams of type 'a'.
    -- The /value/ of the argument should be ignored by any instance of
    -- 'ZoomReadable', so that is safe to pass 'undefined' as the
    -- argument.
    trackIdentifier :: a -> ByteString

    -- | An iteratee to read one value of type 'a' from a stream of 'ByteString'.
    readRaw         :: (Functor m, Monad m)
                    => Iteratee ByteString m a

    -- | An iteratee to read one value of type 'SummaryData a' from a stream
    -- of 'ByteString'.
    readSummary        :: (Functor m, Monad m)
                       => Iteratee ByteString m (SummaryData a)

    -- | Pretty printing, used for dumping values of type 'a'.
    prettyRaw          :: a -> String

    -- | Pretty printing for values of type 'SummaryData a'.
    prettySummaryData  :: SummaryData a -> String

    -- | Delta-decode a list of values
    deltaDecodeRaw :: [a] -> [a]
    deltaDecodeRaw = id

data ZoomRaw = forall a . ZoomReadable a => ZoomRaw [a]

data ZoomSummarySO = forall a . ZoomReadable a => ZoomSummarySO (SummarySO a)

data ZoomSummary = forall a . ZoomReadable a => ZoomSummary (Summary a)

instance Timestampable ZoomSummary where
    timestamp (ZoomSummary s) = timestamp s

data ZoomSummaryUTC = forall a . ZoomReadable a => ZoomSummaryUTC (SummaryUTC a)

instance UTCTimestampable ZoomSummaryUTC where
    utcTimestamp (ZoomSummaryUTC s) = utcTimestamp s

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
    initSummaryWork    :: SampleOffset -> SummaryWork a

    -- | Update a 'SummaryData' with the value of 'a' occuring at the
    -- given 'SampleOffset'.
    updateSummaryData  :: SampleOffset -> a
                       -> SummaryWork a
                       -> SummaryWork a

    -- | Finalize a 'SummaryWork a', generating a 'SummaryData a'.
    toSummaryData      :: SampleOffsetDiff -> SummaryWork a -> SummaryData a

    -- | Append two 'SummaryData'
    appendSummaryData  :: SampleOffsetDiff -> SummaryData a
                       -> SampleOffsetDiff -> SummaryData a
                       -> SummaryData a

    -- | Delta-encode a value.
    deltaEncodeRaw :: SummaryWork a -> a -> a
    deltaEncodeRaw _ = id

data ZoomWork = forall a . (Typeable a, ZoomWritable a) => ZoomWork
    { levels   :: IntMap (SummarySO a)
    , currWork :: Maybe (SummaryWork a)
    }

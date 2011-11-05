{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
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
      TrackType(..)
    , TrackMap
    , TrackSpec(..)
    , IdentifyTrack

    -- * Classes
    , ZoomReadable(..)
    , ZoomWritable(..)

    , ZoomRaw(..)

    , ZoomSummary(..)

    , ZoomWork(..)

    -- * Types
    , Packet(..)
    , Summary(..)
    , SummaryData()

    , summaryDuration

    -- * CacheFile
    , CacheFile(..)
    , mkCacheFile
    , fiFull

) where

import Blaze.ByteString.Builder
import Control.Monad.Trans (MonadIO)
import Data.ByteString (ByteString)
import Data.Dynamic
import Data.Int
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Iteratee (Iteratee)
import qualified Data.Iteratee as I
import qualified Data.ListLike as LL
import Data.Word

import Data.ZoomCache.Common

------------------------------------------------------------
-- | A map of all track numbers to their 'TrackSpec'
type TrackMap = IntMap TrackSpec

-- | A specification of the type and name of each track
data TrackSpec = TrackSpec
    { specType   :: !TrackType
    , specDRType :: !DataRateType
    , specRate   :: {-# UNPACK #-}!Rational
    , specName   :: !ByteString
    }
    deriving (Show)

data TrackType = forall a . ZoomReadable a => TT a

instance Show TrackType where
    show = const "<<TrackType>>"

type IdentifyTrack = ByteString -> Maybe TrackType

------------------------------------------------------------

-- | Global and track headers for a zoom-cache file
data CacheFile = CacheFile
    { cfGlobal :: Global
    , cfSpecs  :: IntMap TrackSpec
    }

-- | Create an empty 'CacheFile' using the given 'Global'
mkCacheFile :: Global -> CacheFile
mkCacheFile g = CacheFile g IM.empty

-- | Determine whether all tracks of a 'CacheFile' are specified
fiFull :: CacheFile -> Bool
fiFull (CacheFile g specs) = IM.size specs == noTracks g

------------------------------------------------------------

data Packet = Packet
    { packetTrack      :: {-# UNPACK #-}!TrackNo
    , packetEntryTime  :: {-# UNPACK #-}!TimeStamp
    , packetExitTime   :: {-# UNPACK #-}!TimeStamp
    , packetCount      :: {-# UNPACK #-}!Int
    , packetData       :: !ZoomRaw
    , packetTimeStamps :: ![TimeStamp]
    }

------------------------------------------------------------
-- | A recorded block of summary data
data Summary a = Summary
    { summaryTrack     :: {-# UNPACK #-}!TrackNo
    , summaryLevel     :: {-# UNPACK #-}!Int
    , summaryEntryTime :: {-# UNPACK #-}!TimeStamp
    , summaryExitTime  :: {-# UNPACK #-}!TimeStamp
    , summaryData      :: !(SummaryData a)
    }

-- | The duration covered by a summary, in units of 1 / the track's datarate
summaryDuration :: Summary a -> Int64
summaryDuration s = (unTS $ summaryExitTime s) - (unTS $ summaryEntryTime s)

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

    -- | An iteratee to read one value of type 'a' from a stream of something
    -- like '[Word8]' or 'ByteString'.
    readRaw         :: (I.Nullable s, LL.ListLike s Word8, Functor m, MonadIO m)
                    => Iteratee s m a

    -- | An iteratee to read one value of type 'SummaryData a' from a stream
    -- of something like '[Word8]' or 'ByteString'.
    readSummary        :: (I.Nullable s, LL.ListLike s Word8, Functor m, MonadIO m)
                       => Iteratee s m (SummaryData a)

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
    updateSummaryData  :: TimeStamp -> a
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

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
-- Types used throughout zoom-cache
----------------------------------------------------------------------

module Data.ZoomCache.Common (
    -- * Types
      TimeStamp(..)
    , TrackType(..)
    , DataRateType(..)
    , TrackNo

    -- * Global header
    , Global(..)

    -- * CacheFile
    , CacheFile(..)
    , mkCacheFile
    , fiFull

    -- * Version
    , Version(..)

    -- * Track specification
    , TrackMap
    , TrackSpec(..)
) where

import qualified Data.ByteString.Lazy as L
import Data.Int
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

------------------------------------------------------------

type TrackNo = Int

data TimeStamp = TS { unTS :: {-# UNPACK #-}!Int64 }
    deriving (Eq, Ord, Show)

data Version = Version !Int !Int
    deriving (Eq, Show)

data Global = Global
    { version          :: Version
    , noTracks         :: Int
    , presentationTime :: Rational
    , baseTime         :: Rational
    , baseUTC          :: Maybe Int -- UTCTime
    }
    deriving (Show)

-- | A map of all track numbers to their 'TrackSpec'
type TrackMap = IntMap TrackSpec

-- | A specification of the type and name of each track
data TrackSpec = TrackSpec
    { specType   :: !TrackType
    , specDRType :: !DataRateType
    , specRate   :: {-# UNPACK #-}!Rational
    , specName   :: !L.ByteString
    }
    deriving (Show)

data TrackType = ZDouble | ZInt
    deriving (Eq, Show)

-- | Constant or Variable datarate.
-- For constant datarate, timestamps are implied as incrementing by 1/datarate
-- For variable datarate, explicit timestamps are attached to each datum, encoded
-- as a separate block of timestamps in the Raw Data packet.
data DataRateType = ConstantDR | VariableDR
    deriving (Show)

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


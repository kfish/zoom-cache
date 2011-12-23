{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      : Data.ZoomCache
-- Copyright   : Conrad Parker
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Conrad Parker <conrad@metadecks.org>
-- Stability   : unstable
-- Portability : unknown
--
-- API for implementing ZoomCache applications
----------------------------------------------------------------------

module Data.ZoomCache (
    -- * TimeStamps
      TimeStamp (..)
    , TimeStampDiff(..)
    , timeStampDiff
    , timeStampFromSO
    , Timestampable(..)
    , before

    , UTCTimestampable(..)
    , beforeUTC
    , timeStampFromUTCTime
    , utcTimeFromTimeStamp

    -- * Types
    , SampleOffset(..)
    , TrackNo
    , Codec(..)
    , IdentifyCodec
    , SampleRateType(..)

    , Global(..)
    , CacheFile(..)

    , ZoomReadable(..)

    , ZoomRaw(..)
    , ZoomSummary(..)
    , Packet(..)
    , Summary(..)

    , ZoomSummaryUTC(..)
    , PacketUTC(..)
    , SummaryUTC(..)

    , ZoomSummarySO(..)
    , PacketSO(..)
    , SummarySO(..)

    -- * Track specification
    , TrackMap
    , TrackSpec(..)

    -- * The ZoomWrite class
    , ZoomWrite(..)
    , ZoomWritable(..)

    -- * The ZoomW monad
    , ZoomW
    , withFileWrite
    , flush

    -- * ZoomWHandle IO functions
    , ZoomWHandle
    , openWrite
    , closeWrite

    -- * Watermarks
    , watermark
    , setWatermark

    -- * TrackSpec helpers
    , setCodec
    , setCodecMultichannel
    , mkTrackSpec
    , oneTrack

    -- * Standard identifiers
    , standardIdentifiers

    -- * Iteratee parsers
    , module Data.Iteratee.ZoomCache

    -- * Pretty printing
    , module Data.ZoomCache.Pretty
) where

import Data.Int
import Data.Word

import Data.ZoomCache.Write

import Data.Iteratee.ZoomCache
import Data.ZoomCache.Common
import Data.ZoomCache.Identify
import Data.ZoomCache.Pretty
import Data.ZoomCache.TrackSpec
import Data.ZoomCache.Types

-- Track Types
import Data.ZoomCache.Bool()
import Data.ZoomCache.Unit()
import Data.ZoomCache.Numeric.IEEE754()
import Data.ZoomCache.Numeric.Int()
import Data.ZoomCache.Numeric.Word()

----------------------------------------------------------------------

-- | 'IdentifyTrack' functions provided for standard codecs provided
-- by the zoom-cache library.
standardIdentifiers :: [IdentifyCodec]
standardIdentifiers =
    [ identifyCodec (undefined :: Float)
    , identifyCodec (undefined :: Double)
    , identifyCodec (undefined :: Int)
    , identifyCodec (undefined :: Int8)
    , identifyCodec (undefined :: Int16)
    , identifyCodec (undefined :: Int32)
    , identifyCodec (undefined :: Int64)
    , identifyCodec (undefined :: Word)
    , identifyCodec (undefined :: Word8)
    , identifyCodec (undefined :: Word16)
    , identifyCodec (undefined :: Word32)
    , identifyCodec (undefined :: Word64)
    , identifyCodec (undefined :: Integer)
    , identifyCodec (undefined :: ())
    , identifyCodec (undefined :: Bool)
    ]


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
    -- * Types
      TimeStamp(..)
    , TrackNo
    , Codec
    , IdentifyCodec
    , DataRateType(..)

    , CacheFile(..)

    , ZoomReadable(..)

    , ZoomRaw(..)
    , ZoomSummary(..)
    , Packet(..)
    , Summary(..)

    -- * Track specification
    , TrackMap
    , TrackSpec(..)

    -- * The ZoomWrite class
    , ZoomWrite(..)

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
    , oneTrack

    -- * Standard identifiers
    , standardIdentifiers

    -- * Iteratee parsers
    , module Data.Iteratee.ZoomCache

    -- * Pretty printing
    , module Data.ZoomCache.Pretty
) where

import Data.Int

import Data.ZoomCache.Write

import Data.Iteratee.ZoomCache
import Data.ZoomCache.Common
import Data.ZoomCache.Identify
import Data.ZoomCache.Pretty
import Data.ZoomCache.Types

-- Track Types
import Data.ZoomCache.Bool()
import Data.ZoomCache.Unit()
import Data.ZoomCache.Numeric.IEEE754()
import Data.ZoomCache.Numeric.Int()

----------------------------------------------------------------------

-- | 'IdentifyTrack' functions provided for standard codecs provided
-- by the zoom-cache library.
standardIdentifiers :: [IdentifyCodec]
standardIdentifiers =
    [ identifyCodec (undefined :: Float)
    , identifyCodec (undefined :: Double)
    , identifyCodec (undefined :: Int)
    , identifyCodec (undefined :: Int32)
    , identifyCodec (undefined :: ())
    , identifyCodec (undefined :: Bool)
    ]


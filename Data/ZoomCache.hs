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
    , TrackType
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

    -- * Iteratee parsers
    , module Data.Iteratee.ZoomCache

    -- * Pretty printing
    , module Data.ZoomCache.Pretty
) where

import Data.ZoomCache.Write

import Data.Iteratee.ZoomCache
import Data.ZoomCache.Common
import Data.ZoomCache.Pretty
import Data.ZoomCache.Types

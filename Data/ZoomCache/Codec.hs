{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      : Data.ZoomCache.Codec
-- Copyright   : Conrad Parker
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Conrad Parker <conrad@metadecks.org>
-- Stability   : unstable
-- Portability : unknown
--
-- This module re-exports the required interfaces and some useful
-- functions for developing zoom-cache codecs.
--
-- To implement a codec, specify 'SummaryData' and 'SummaryWork' types, and
-- implement the methods of the ZoomReadable and ZoomWritable classes.
--
-- For sample implementations, read the source of the provided instances
-- "Data.ZoomCache.Numeric.Int" and "Data.ZoomCache.Numeric.Double".
----------------------------------------------------------------------

module Data.ZoomCache.Codec (
    -- * Required interfaces
      ZoomReadable(..)
    , ZoomWritable(..)
    , ZoomWrite(..)

    -- * Identification
    , IdentifyCodec
    , identifyCodec

    -- * Raw data reading iteratees
    , readInt8
    , readInt16be
    , readInt32be
    , readInt64be
    , readWord8
    , readWord16be
    , readWord32be
    , readWord64be
    , readIntegerVLC
    , readFloat32be
    , readDouble64be
    , readRational64be

    -- * ZoomWrite instance helpers
    , writeData
    , writeDataVBR
    , writeDataTS
    , ZoomW

    -- * Builders
    , fromRational64
    , fromIntegral32be
    , fromIntegerVLC
    , fromFloat
    , fromDouble

    -- * ZoomCache Types
    , Codec
    , TimeStamp (..)
    , TimeStampDiff(..)
    , timeStampDiff
    , timeStampFromSO
    , SampleOffset(..)
    , SampleOffsetDiff(..)
    , sampleOffsetDiff
    , TrackNo

    -- * Delta encoding
    , module Data.ZoomCache.Numeric.Delta
    -- * Minimum and maximum floating point
    , module Data.ZoomCache.Numeric.FloatMinMax
) where

import Blaze.ByteString.Builder.ZoomCache
import Data.Iteratee.ZoomCache.Utils
import Data.ZoomCache.Common
import Data.ZoomCache.Identify
import Data.ZoomCache.Types
import Data.ZoomCache.Write
import Data.ZoomCache.Numeric.Delta
import Data.ZoomCache.Numeric.FloatMinMax

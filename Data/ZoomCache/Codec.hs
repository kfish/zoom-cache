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
-- To implement a codec, specify 'RawData' and 'SummaryData' types, and
-- implement the methods of the ZoomReadable and ZoomWritable classes.
--
-- For sample implementations, read the source of the provided instances
-- "Data.ZoomCache.Int" and "Data.ZoomCache.Double".
----------------------------------------------------------------------

module Data.ZoomCache.Codec (
    -- * Required interfaces
      ZoomReadable(..)
    , RawData()
    , SummaryData()
    , ZoomWritable(..)
    , ZoomWrite(..)

    -- * Raw data reading iteratees
    , readInt16be
    , readInt32be
    , readInt64be
    , readDouble64be
    , readRational64be

    -- * ZoomWrite instance helpers
    , writeData
    , writeDataVBR

    -- * Builders
    , fromRational64
    , fromIntegral32be
    , fromDouble

    -- * ZoomCache Types
    , TimeStamp(..)
) where

import Blaze.ByteString.Builder.ZoomCache
import Data.Iteratee.ZoomCache.Utils
import Data.ZoomCache.Common
import Data.ZoomCache.Types
import Data.ZoomCache.Write

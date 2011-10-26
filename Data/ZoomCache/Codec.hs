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
-- Interface for implementing ZoomCache codec instances.
-- This module re-exports the interfaces required for developing
-- zoom-cache codecs.
--
-- For sample implementations, read the source of the provided instances
-- "Data.ZoomCache.Int" and "Data.ZoomCache.Double".
----------------------------------------------------------------------

module Data.ZoomCache.Codec (
    -- * Interfaces
      ZoomRead(..)
    , PacketData()
    , ZoomSummary(..)
    , SummaryData()
    , ZoomSummaryWrite(..)
    , ZoomWrite(..)

    -- * ZoomCache Types
    , TimeStamp(..)

    -- * Raw data iteratees
    , zReadInt16
    , zReadInt32
    , zReadInt64
    , zReadFloat64be
    , readRational64

    -- * Binary data helpers
    , fromRational64
    , encInt
    , encInt64
    , encDbl
    , toWord64

    -- * Write instance helpers
    , writeData
    , writeDataVBR
) where

import Blaze.ByteString.Builder.ZoomCache
import Data.Iteratee.ZoomCache.Utils
import Data.ZoomCache.Common
import Data.ZoomCache.Types
import Data.ZoomCache.Write

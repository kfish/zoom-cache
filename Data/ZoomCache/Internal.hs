{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      : Data.ZoomCache.Internal
-- Copyright   : Conrad Parker
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Conrad Parker <conrad@metadecks.org>
-- Stability   : unstable
-- Portability : unknown
--
-- Library-internal definitions
----------------------------------------------------------------------

module Data.ZoomCache.Internal (
    globalHeader
  , versionMajor
  , versionMinor

  -- * Track header
  , trackHeader

  -- * Packet header
  , packetHeader

  -- * Summary header
  , summaryHeader
) where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC

------------------------------------------------------------
-- Magic

-- | Magic identifier at the beginning of a zoom-cache file.
globalHeader :: L.ByteString
globalHeader = LC.pack "\xe5ZXhe4d\0"

-- | The major version encoded by this library
versionMajor :: Int
versionMajor = 1

-- | The minor version encoded by this library
versionMinor :: Int
versionMinor = 0

-- | Identifier for track headers
trackHeader :: L.ByteString
trackHeader = LC.pack "\xe5ZXtRcK\0"

-- | Identifier for packet headers
packetHeader :: L.ByteString
packetHeader = LC.pack "\xe5ZXp4ck\0"

-- | Identifier for summary headers
summaryHeader :: L.ByteString
summaryHeader = LC.pack "\xe5ZX5umm\0"


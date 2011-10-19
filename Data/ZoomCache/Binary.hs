{-# LANGUAGE RecordWildCards #-}
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
-- Blaze-builder utility functions for writing ZoomCache files.
----------------------------------------------------------------------

module Data.ZoomCache.Binary (
    -- * Builders
      fromGlobal
    , fromTrackType
    , fromDataRateType
    , fromTrackNo
    , fromSummary
) where

import Blaze.ByteString.Builder
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Monoid

import Data.ZoomCache.Common
import Data.ZoomCache.Types

----------------------------------------------------------------------
-- Builders for local types

fromGlobal :: Global -> Builder
fromGlobal Global{..} = mconcat
    [ fromLazyByteString globalHeader
    , mconcat $
        [ fromVersion version
        , encInt noTracks
        , fromRational64 presentationTime
        , fromRational64 baseTime
        ]
    , fromLazyByteString $ LC.pack (replicate 20 '\0') -- UTCTime
    ]

fromVersion :: Version -> Builder
fromVersion (Version vMaj vMin) = mconcat
    [ fromInt16be . fromIntegral $ vMaj
    , fromInt16be . fromIntegral $ vMin
    ]

fromTrackType :: TrackType -> Builder
fromTrackType ZDouble = fromInt16be 0
fromTrackType ZInt    = fromInt16be 1

fromDataRateType :: DataRateType -> Builder
fromDataRateType ConstantDR = fromInt16be 0
fromDataRateType VariableDR = fromInt16be 1

fromTrackNo :: TrackNo -> Builder
fromTrackNo = fromInt32be . fromIntegral

fromSummary :: (ZoomSummaryWrite a) => Summary a -> Builder
fromSummary s@Summary{..} = mconcat [ fromSummaryHeader s, l, d]
    where
        d = fromSummaryData summaryData
        l = encInt . L.length . toLazyByteString $ d

fromSummaryHeader :: Summary a -> Builder
fromSummaryHeader s = mconcat
    [ fromLazyByteString summaryHeader
    , encInt . summaryTrack $ s
    , encInt . summaryLevel $ s
    , encInt64 . unTS . summaryEntryTime $ s
    , encInt64 . unTS . summaryExitTime $ s
    ]


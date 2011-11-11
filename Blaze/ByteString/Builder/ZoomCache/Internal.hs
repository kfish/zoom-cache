{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      : Blaze.ByteString.Builder.ZoomCache.Internal
-- Copyright   : Conrad Parker
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Conrad Parker <conrad@metadecks.org>
-- Stability   : unstable
-- Portability : unknown
--
-- Blaze-builder utility functions for writing ZoomCache files.
----------------------------------------------------------------------

module Blaze.ByteString.Builder.ZoomCache.Internal (
    -- * Builders
      fromFlags
    , fromGlobal
    , fromSummary
    , fromTrackNo
    , fromCodec
) where

import Blaze.ByteString.Builder
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Monoid

import Blaze.ByteString.Builder.ZoomCache
import Data.ZoomCache.Common
import Data.ZoomCache.Format
import Data.ZoomCache.Types

----------------------------------------------------------------------
-- Creating builders for ZoomCache types.

fromFlags :: Bool -> DataRateType -> Builder
fromFlags delta drType = fromInt16be (dl .|. dr)
    where
        dl | delta                = 2
           | otherwise            = 0
        dr | drType == VariableDR = 1
           | otherwise            = 0

fromGlobal :: Global -> Builder
fromGlobal Global{..} = mconcat
    [ fromByteString globalHeader
    , mconcat $
        [ fromVersion version
        , fromIntegral32be noTracks
        , fromRational64 presentationTime
        , fromRational64 baseTime
        ]
    , fromByteString $ C.pack (replicate 20 '\0') -- UTCTime
    ]

fromSummary :: ZoomWritable a => Summary a -> Builder
fromSummary s@Summary{..} = mconcat [ fromSummaryHeader s, l, d]
    where
        d = fromSummaryData summaryData
        l = fromIntegral32be . B.length . toByteString $ d

fromSummaryHeader :: Summary a -> Builder
fromSummaryHeader s = mconcat
    [ fromByteString summaryHeader
    , fromIntegral32be . summaryTrack $ s
    , fromIntegral32be . summaryLevel $ s
    , fromTimeStamp . summaryEntryTime $ s
    , fromTimeStamp . summaryExitTime $ s
    ]

fromTrackNo :: TrackNo -> Builder
fromTrackNo = fromInt32be . fromIntegral

fromCodec :: Codec -> Builder
fromCodec (Codec a) = fromByteString $ trackIdentifier a

fromVersion :: Version -> Builder
fromVersion (Version vMaj vMin) = mconcat
    [ fromInt16be . fromIntegral $ vMaj
    , fromInt16be . fromIntegral $ vMin
    ]


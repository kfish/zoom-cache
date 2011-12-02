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
    , fromSummarySO
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

fromFlags :: Bool -> Bool -> SampleRateType -> Builder
fromFlags delta zlib drType = fromInt16be (zl .|. dl .|. dr)
    where
        zl | zlib                 = 4
           | otherwise            = 0
        dl | delta                = 2
           | otherwise            = 0
        dr | drType == VariableSR = 1
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

fromSummarySO :: ZoomWritable a => SummarySO a -> Builder
fromSummarySO s@SummarySO{..} = mconcat [ fromSummarySOHeader s, l, d]
    where
        d = fromSummaryData summarySOData
        l = fromIntegral32be . B.length . toByteString $ d

fromSummarySOHeader :: SummarySO a -> Builder
fromSummarySOHeader s = mconcat
    [ fromByteString summaryHeader
    , fromIntegral32be . summarySOTrack $ s
    , fromIntegral32be . summarySOLevel $ s
    , fromSampleOffset . summarySOEntry $ s
    , fromSampleOffset . summarySOExit $ s
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


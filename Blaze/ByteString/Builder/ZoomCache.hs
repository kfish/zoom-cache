{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      : Blaze.ByteString.Builder.ZoomCache
-- Copyright   : Conrad Parker
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Conrad Parker <conrad@metadecks.org>
-- Stability   : unstable
-- Portability : unknown
--
-- Blaze-builder utility functions for writing ZoomCache files.
----------------------------------------------------------------------

module Blaze.ByteString.Builder.ZoomCache (
    -- * Builders
      fromDataRateType
    , fromGlobal
    , fromSummary
    , fromTimeStamp
    , fromTrackNo
    , fromTrackType

    -- * Builder helpers
    , fromDouble
    , fromIntegral32be
    , fromRational64
) where

import Blaze.ByteString.Builder
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Monoid
import Data.Ratio
import Data.Word
import Unsafe.Coerce (unsafeCoerce)

import Data.ZoomCache.Common
import Data.ZoomCache.Types

----------------------------------------------------------------------
-- Creating builders for ZoomCache types.

fromDataRateType :: DataRateType -> Builder
fromDataRateType ConstantDR = fromInt16be 0
fromDataRateType VariableDR = fromInt16be 1

fromGlobal :: Global -> Builder
fromGlobal Global{..} = mconcat
    [ fromLazyByteString globalHeader
    , mconcat $
        [ fromVersion version
        , fromIntegral32be noTracks
        , fromRational64 presentationTime
        , fromRational64 baseTime
        ]
    , fromLazyByteString $ LC.pack (replicate 20 '\0') -- UTCTime
    ]

fromSummary :: (ZoomSummaryWrite a) => Summary a -> Builder
fromSummary s@Summary{..} = mconcat [ fromSummaryHeader s, l, d]
    where
        d = fromSummaryData summaryData
        l = fromIntegral32be . L.length . toLazyByteString $ d

fromSummaryHeader :: Summary a -> Builder
fromSummaryHeader s = mconcat
    [ fromLazyByteString summaryHeader
    , fromIntegral32be . summaryTrack $ s
    , fromIntegral32be . summaryLevel $ s
    , fromTimeStamp . summaryEntryTime $ s
    , fromTimeStamp . summaryExitTime $ s
    ]

-- | Serialize a 'TimeStamp' in 64bit big endian format.
fromTimeStamp :: TimeStamp -> Builder
fromTimeStamp = fromInt64be . fromIntegral . unTS

fromTrackNo :: TrackNo -> Builder
fromTrackNo = fromInt32be . fromIntegral

fromTrackType :: TrackType -> Builder
fromTrackType ZDouble = fromInt16be 0
fromTrackType ZInt    = fromInt16be 1

fromVersion :: Version -> Builder
fromVersion (Version vMaj vMin) = mconcat
    [ fromInt16be . fromIntegral $ vMaj
    , fromInt16be . fromIntegral $ vMin
    ]

----------------------------------------------------------------------
-- Creating builders from numeric types used by ZoomCache.

-- | Serialize a 'Double' in big-endian IEEE 754-2008 binary64 format
-- (IEEE 754-1985 double format).
fromDouble :: Double -> Builder
fromDouble = fromWord64be . toWord64
    where
        toWord64 :: Double -> Word64
        toWord64 = unsafeCoerce

-- | Serialize an 'Integral' in 32bit big endian format.
fromIntegral32be :: forall a . (Integral a) => a -> Builder
fromIntegral32be = fromInt32be . fromIntegral

-- | Serialize a 'Rational' as a sequence of two 64bit big endian format
-- integers.
fromRational64 :: Rational -> Builder
fromRational64 r = mconcat
    [ fromInt64be . fromIntegral . numerator $ r
    , fromInt64be . fromIntegral . denominator $ r
    ]


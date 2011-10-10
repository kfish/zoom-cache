{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}

module Data.ZoomCache.Binary (
    -- * Builders
      encInt
    , encDbl
    , fromRational64
    , fromGlobal
    , fromTrackType
    , fromDataRateType
    , fromTrackNo
    , fromSummary

    -- * Helpers
    , toWord64
) where

import Blaze.ByteString.Builder hiding (flush)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Monoid
import Data.Ratio
import Data.Word
import Unsafe.Coerce (unsafeCoerce)

import Data.ZoomCache.Common
import Data.ZoomCache.Summary

----------------------------------------------------------------------
--

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

fromSummary :: Summary -> Builder
fromSummary s@SummaryDouble{..} = mconcat [ fromSummaryHeader s, l, d]
    where
        d = mconcat $ map encDbl
            [ summaryDoubleEntry
            , summaryDoubleExit
            , summaryDoubleMin
            , summaryDoubleMax
            , summaryAvg
            , summaryRMS
            ]
        l = encInt . L.length . toLazyByteString $ d
fromSummary s@SummaryInt{..} = mconcat [ fromSummaryHeader s, l, d]
    where
        d = mconcat $ map encInt
            [ summaryIntEntry
            , summaryIntExit
            , summaryIntMin
            , summaryIntMax
            ] ++ map encDbl
            [ summaryAvg
            , summaryRMS
            ]
        l = encInt . L.length . toLazyByteString $ d

fromSummaryHeader :: Summary -> Builder
fromSummaryHeader s = mconcat
    [ fromLazyByteString summaryHeader
    , encInt . summaryTrack $ s
    , encInt . summaryLevel $ s
    , encInt . unTS . summaryEntryTime $ s
    , encInt . unTS . summaryExitTime $ s
    ]

----------------------------------------------------------------------
-- Binary data helpers
    
fromRational64 :: Rational -> Builder
fromRational64 r = mconcat
    [ fromInt64be . fromIntegral . numerator $ r
    , fromInt64be . fromIntegral . denominator $ r
    ]

encInt :: forall a . (Integral a) => a -> Builder
encInt = fromInt32be . fromIntegral

encDbl :: Double -> Builder
encDbl = fromWord64be . toWord64

toWord64 :: Double -> Word64
toWord64 = unsafeCoerce


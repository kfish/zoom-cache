{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}

module Data.ZoomCache.Binary (
    -- * Builders
      encInt
    , encDbl
    , fromTrackType
    , fromTrackNo
    , fromSummary

    -- * Helpers
    , toWord64
) where

import Blaze.ByteString.Builder hiding (flush)
import qualified Data.ByteString.Lazy as L
import Data.Monoid
import Data.Word
import Unsafe.Coerce (unsafeCoerce)

import Data.ZoomCache.Common
import Data.ZoomCache.Summary

----------------------------------------------------------------------
--

fromTrackType :: TrackType -> Builder
fromTrackType ZDouble = fromInt32be 0
fromTrackType ZInt    = fromInt32be 1

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
    
encInt :: forall a . (Integral a) => a -> Builder
encInt = fromInt32be . fromIntegral

encDbl :: Double -> Builder
encDbl = fromWord64be . toWord64

toWord64 :: Double -> Word64
toWord64 = unsafeCoerce


{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}

module Data.ZoomCache.Binary (
      buildInt16
    , buildInt32
    , buildInt64
    , encInt
    , encDbl
    , toWord64

    -- * Builders
    , fromTrackType
    , fromTrackNo
    , fromSummary
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
    , encInt . summaryEntryTime $ s
    , encInt . summaryExitTime $ s
    ]

----------------------------------------------------------------------
-- Binary data helpers
    
buildInt16 :: Int -> L.ByteString
buildInt16 = toLazyByteString . fromInt16be . fromIntegral

buildInt32 :: Int -> L.ByteString
buildInt32 = toLazyByteString . fromInt32be . fromIntegral

buildInt64 :: Integer -> L.ByteString
buildInt64 = toLazyByteString . fromInt64be . fromIntegral

encInt :: forall a . (Integral a) => a -> Builder
encInt = fromInt32be . fromIntegral

encDbl :: Double -> Builder
encDbl = fromWord64be . toWord64

toWord64 :: Double -> Word64
toWord64 = unsafeCoerce


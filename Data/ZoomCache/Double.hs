{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
{- |
   Module      : Data.ZoomCache.Double
   Copyright   : Conrad Parker
   License     : BSD3-style (see LICENSE)

   Maintainer  : Conrad Parker <conrad@metadecks.org>
   Stability   : unstable
   Portability : unknown

Default codec implementation for values of type Double. This module
implements the interfaces documented in "Data.ZoomCache.Codec".
View the module source for enlightenment.

The table below describes the encoding of SummaryData for Double.

@
   | ...                                                           |   -35
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Entry (double)                                                | 36-39
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 40-43
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Exit (double)                                                 | 44-47
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 48-51
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Min (double)                                                  | 52-55
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 56-59
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Max (double)                                                  | 60-63
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 64-67
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Avg (double)                                                  | 68-71
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 72-75
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | RMS (double)                                                  | 76-79
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 80-83
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
@

Field encoding formats:

  @double@: big-endian IEEE 754-2008 binary64 (IEEE 754-1985 double)

-}
----------------------------------------------------------------------

module Data.ZoomCache.Double (
      SummaryData(..)
    , SummaryWork(..)
)where

import Blaze.ByteString.Builder
import Control.Monad (replicateM)
import Control.Monad.Trans (MonadIO)
import qualified Data.ByteString.Lazy as L
import Data.Iteratee (Iteratee)
import Data.Monoid
import Data.Word
import Text.Printf

import Data.ZoomCache.Codec
import Numeric.FloatMinMax

----------------------------------------------------------------------

-- Identifier for track headers
trackTypeDouble :: L.ByteString
trackTypeDouble = "ZOOMf64b"

----------------------------------------------------------------------
-- Read

instance ZoomReadable Double where
    data SummaryData Double = SummaryDouble
        { summaryDoubleEntry :: {-# UNPACK #-}!Double
        , summaryDoubleExit  :: {-# UNPACK #-}!Double
        , summaryDoubleMin   :: {-# UNPACK #-}!Double
        , summaryDoubleMax   :: {-# UNPACK #-}!Double
        , summaryDoubleAvg   :: {-# UNPACK #-}!Double
        , summaryDoubleRMS   :: {-# UNPACK #-}!Double
        }

    trackIdentifier = const trackTypeDouble

    readRaw     = readDouble64be
    readSummary = readSummaryDouble

    prettyRaw         = prettyPacketDouble
    prettySummaryData = prettySummaryDouble

prettyPacketDouble :: Double -> String
prettyPacketDouble = printf "%.3f"

readSummaryDouble :: (Functor m, MonadIO m)
                  => Iteratee [Word8] m (SummaryData Double)
readSummaryDouble = do
    [en,ex,mn,mx,avg,rms] <- replicateM 6 readDouble64be
    return (SummaryDouble en ex mn mx avg rms)

prettySummaryDouble :: SummaryData Double -> String
prettySummaryDouble SummaryDouble{..} = concat
    [ printf "\tentry: %.3f\texit: %.3f\tmin: %.3f\tmax: %.3f\t"
          summaryDoubleEntry summaryDoubleExit summaryDoubleMin summaryDoubleMax
    , printf "avg: %.3f\trms: %.3f" summaryDoubleAvg summaryDoubleRMS
    ]

{-
    typeOfSummaryData = typeOfSummaryDouble

typeOfSummaryDouble :: SummaryData Double -> TypeRep
typeOfSummaryDouble _ = mkTyConApp tyCon [d,d,d,d]
    where
        tyCon = mkTyCon3 "zoom-cache" "Data.ZoomCache.Types" "SummaryDouble"
        d = typeOf (undefined :: Double)
-}

----------------------------------------------------------------------
-- Write

instance ZoomWrite Double where
    write = writeData

instance ZoomWrite (TimeStamp, Double) where
    write = writeDataVBR

instance ZoomWritable Double where
    data SummaryWork Double = SummaryWorkDouble
        { ztsdTime  :: {-# UNPACK #-}!TimeStamp
        , ztsdEntry :: {-# UNPACK #-}!Double
        , ztsdExit  :: {-# UNPACK #-}!Double
        , ztsdMin   :: {-# UNPACK #-}!Double
        , ztsdMax   :: {-# UNPACK #-}!Double
        , ztsdSum   :: {-# UNPACK #-}!Double
        , ztsdSumSq :: {-# UNPACK #-}!Double
        }
    fromRaw           = fromDouble
    fromSummaryData   = fromSummaryDouble

    initSummaryWork   = initSummaryDouble
    toSummaryData     = mkSummaryDouble
    updateSummaryData = updateSummaryDouble
    appendSummaryData = appendSummaryDouble

initSummaryDouble :: TimeStamp -> SummaryWork Double
initSummaryDouble entry = SummaryWorkDouble
    { ztsdTime = entry
    , ztsdEntry = 0.0
    , ztsdExit = 0.0
    , ztsdMin = floatMax
    , ztsdMax = negate floatMax
    , ztsdSum = 0.0
    , ztsdSumSq = 0.0
    }

mkSummaryDouble :: Double -> SummaryWork Double -> SummaryData Double
mkSummaryDouble dur SummaryWorkDouble{..} = SummaryDouble
    { summaryDoubleEntry = ztsdEntry
    , summaryDoubleExit = ztsdExit
    , summaryDoubleMin = ztsdMin
    , summaryDoubleMax = ztsdMax
    , summaryDoubleAvg = ztsdSum / dur
    , summaryDoubleRMS = sqrt $ ztsdSumSq / dur
    }

fromSummaryDouble :: SummaryData Double -> Builder
fromSummaryDouble SummaryDouble{..} = mconcat $ map fromDouble
    [ summaryDoubleEntry
    , summaryDoubleExit
    , summaryDoubleMin
    , summaryDoubleMax
    , summaryDoubleAvg
    , summaryDoubleRMS
    ]

updateSummaryDouble :: Int -> TimeStamp -> Double -> SummaryWork Double
                    -> SummaryWork Double
updateSummaryDouble count t d SummaryWorkDouble{..} = SummaryWorkDouble
    { ztsdTime = t
    , ztsdEntry = if count == 0 then d else ztsdEntry
    , ztsdExit = d
    , ztsdMin = min ztsdMin d
    , ztsdMax = max ztsdMax d
    , ztsdSum = ztsdSum + (d * dur)
    , ztsdSumSq = ztsdSumSq + (d*d * dur)
    }
    where
        !dur = fromIntegral $ (unTS t) - (unTS ztsdTime)

appendSummaryDouble :: Double -> SummaryData Double
                    -> Double -> SummaryData Double
                    -> SummaryData Double
appendSummaryDouble dur1 s1 dur2 s2 = SummaryDouble
    { summaryDoubleEntry = summaryDoubleEntry s1
    , summaryDoubleExit = summaryDoubleExit s2
    , summaryDoubleMin = min (summaryDoubleMin s1) (summaryDoubleMin s2)
    , summaryDoubleMax = max (summaryDoubleMax s1) (summaryDoubleMax s2)
    , summaryDoubleAvg = ((summaryDoubleAvg s1 * dur1) +
                          (summaryDoubleAvg s2 * dur2)) /
                         durSum
    , summaryDoubleRMS = sqrt $ ((summaryDoubleRMS s1 * summaryDoubleRMS s1 * dur1) +
                                 (summaryDoubleRMS s2 * summaryDoubleRMS s2 * dur2)) /
                                durSum
    }
    where
        !durSum = dur1 + dur2


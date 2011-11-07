{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Iteratee (Iteratee)
import qualified Data.Iteratee as I
import qualified Data.ListLike as LL
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Word
import Text.Printf

import Data.ZoomCache.Codec
import Numeric.FloatMinMax

----------------------------------------------------------------------

-- Identifier for track headers
trackTypeDouble :: ByteString
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

readSummaryDouble :: (I.Nullable s, LL.ListLike s Word8, Functor m, MonadIO m)
                  => Iteratee s m (SummaryData Double)
readSummaryDouble = do
    [en,ex,mn,mx,avg,rms] <- replicateM 6 readDouble64be
    return (SummaryDouble en ex mn mx avg rms)
{-# SPECIALIZE INLINE readSummaryDouble :: (Functor m, MonadIO m) => Iteratee [Word8] m (SummaryData Double) #-}
{-# SPECIALIZE INLINE readSummaryDouble :: (Functor m, MonadIO m) => Iteratee B.ByteString m (SummaryData Double) #-}

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
        { swDoubleTime  :: {-# UNPACK #-}!TimeStamp
        , swDoubleEntry :: !(Maybe Double)
        , swDoubleExit  :: {-# UNPACK #-}!Double
        , swDoubleMin   :: {-# UNPACK #-}!Double
        , swDoubleMax   :: {-# UNPACK #-}!Double
        , swDoubleSum   :: {-# UNPACK #-}!Double
        , swDoubleSumSq :: {-# UNPACK #-}!Double
        }
    fromRaw           = fromDouble
    fromSummaryData   = fromSummaryDouble

    initSummaryWork   = initSummaryDouble
    toSummaryData     = mkSummaryDouble
    updateSummaryData = updateSummaryDouble
    appendSummaryData = appendSummaryDouble

initSummaryDouble :: TimeStamp -> SummaryWork Double
initSummaryDouble entry = SummaryWorkDouble
    { swDoubleTime = entry
    , swDoubleEntry = Nothing
    , swDoubleExit = 0.0
    , swDoubleMin = floatMax
    , swDoubleMax = negate floatMax
    , swDoubleSum = 0.0
    , swDoubleSumSq = 0.0
    }

mkSummaryDouble :: TimeStampDiff -> SummaryWork Double -> SummaryData Double
mkSummaryDouble (TSDiff dur) SummaryWorkDouble{..} = SummaryDouble
    { summaryDoubleEntry = fromMaybe 0.0 swDoubleEntry
    , summaryDoubleExit = swDoubleExit
    , summaryDoubleMin = swDoubleMin
    , summaryDoubleMax = swDoubleMax
    , summaryDoubleAvg = swDoubleSum / fromIntegral dur
    , summaryDoubleRMS = sqrt $ swDoubleSumSq / fromIntegral dur
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

updateSummaryDouble :: TimeStamp -> Double -> SummaryWork Double
                    -> SummaryWork Double
updateSummaryDouble t d SummaryWorkDouble{..} = SummaryWorkDouble
    { swDoubleTime = t
    , swDoubleEntry = Just $ fromMaybe d swDoubleEntry
    , swDoubleExit = d
    , swDoubleMin = min swDoubleMin d
    , swDoubleMax = max swDoubleMax d
    , swDoubleSum = swDoubleSum + (d * dur)
    , swDoubleSumSq = swDoubleSumSq + (d*d * dur)
    }
    where
        !dur = fromIntegral $ (unTS t) - (unTS swDoubleTime)

appendSummaryDouble :: TimeStampDiff -> SummaryData Double
                    -> TimeStampDiff -> SummaryData Double
                    -> SummaryData Double
appendSummaryDouble (TSDiff dur1) s1 (TSDiff dur2) s2 = SummaryDouble
    { summaryDoubleEntry = summaryDoubleEntry s1
    , summaryDoubleExit = summaryDoubleExit s2
    , summaryDoubleMin = min (summaryDoubleMin s1) (summaryDoubleMin s2)
    , summaryDoubleMax = max (summaryDoubleMax s1) (summaryDoubleMax s2)
    , summaryDoubleAvg = ((summaryDoubleAvg s1 * fromIntegral dur1) +
                          (summaryDoubleAvg s2 * fromIntegral dur2)) /
                         fromIntegral durSum
    , summaryDoubleRMS = sqrt $ ((summaryDoubleRMS s1 * summaryDoubleRMS s1 *
                                  fromIntegral dur1) +
                                 (summaryDoubleRMS s2 * summaryDoubleRMS s2 *
                                  fromIntegral dur2)) /
                                fromIntegral durSum
    }
    where
        !durSum = dur1 + dur2


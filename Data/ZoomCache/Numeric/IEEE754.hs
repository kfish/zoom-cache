{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
{- |
   Module      : Data.ZoomCache.Numeric.IEEE754
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

module Data.ZoomCache.Numeric.IEEE754 (
      SummaryData(..)
    , SummaryWork(..)
)where

import Blaze.ByteString.Builder
import Control.Monad.Trans (MonadIO)
import Data.ByteString (ByteString)
import Data.Iteratee (Iteratee)
import Data.Word
import Text.Printf

import Data.ZoomCache.Codec
import Data.ZoomCache.Numeric.Internal
import Data.ZoomCache.Numeric.Types

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
    readSummary = readSummaryNum

    prettyRaw         = prettyPacketDouble
    prettySummaryData = prettySummaryDouble

{-# SPECIALIZE readSummaryNum :: (Functor m, MonadIO m) => Iteratee [Word8] m (SummaryData Double) #-}
{-# SPECIALIZE readSummaryNum :: (Functor m, MonadIO m) => Iteratee ByteString m (SummaryData Double) #-}

prettyPacketDouble :: Double -> String
prettyPacketDouble = printf "%.3f"

prettySummaryDouble :: SummaryData Double -> String
prettySummaryDouble SummaryDouble{..} = concat
    [ printf "\tentry: %.3f\texit: %.3f\tmin: %.3f\tmax: %.3f\t"
          summaryDoubleEntry summaryDoubleExit summaryDoubleMin summaryDoubleMax
    , printf "avg: %.3f\trms: %.3f" summaryDoubleAvg summaryDoubleRMS
    ]

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
    fromSummaryData   = fromSummaryNum

    initSummaryWork   = initSummaryDouble
    toSummaryData     = mkSummaryNum
    updateSummaryData = updateSummaryNum
    appendSummaryData = appendSummaryNum

instance ZoomNum Double where
    numEntry = summaryDoubleEntry
    numExit  = summaryDoubleExit
    numMin   = summaryDoubleMin
    numMax   = summaryDoubleMax
    numAvg   = summaryDoubleAvg
    numRMS   = summaryDoubleRMS

    numWorkTime  = swDoubleTime
    numWorkEntry = swDoubleEntry
    numWorkExit  = swDoubleExit
    numWorkMin   = swDoubleMin
    numWorkMax   = swDoubleMax
    numWorkSum   = swDoubleSum
    numWorkSumSq = swDoubleSumSq

    numMkSummary = SummaryDouble
    numMkSummaryWork = SummaryWorkDouble

{-# SPECIALIZE fromSummaryNum :: SummaryData Double -> Builder #-}
{-# SPECIALIZE mkSummaryNum :: TimeStampDiff -> SummaryWork Double -> SummaryData Double #-}
{-# SPECIALIZE appendSummaryNum :: TimeStampDiff -> SummaryData Double -> TimeStampDiff -> SummaryData Double -> SummaryData Double #-}
{-# SPECIALIZE updateSummaryNum :: TimeStamp -> Double -> SummaryWork Double -> SummaryWork Double #-}

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

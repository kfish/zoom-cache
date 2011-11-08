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

Default codec implementation for values of type Float and Double.
This module implements the interfaces documented in "Data.ZoomCache.Codec".
View the module source for enlightenment.

The table below describes the encoding of SummaryData for Float.

@
   | ...                                                           |   -35
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Entry (float)                                                 | 36-39
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Exit (float)                                                  | 40-43
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Min (float)                                                   | 44-47
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Max (float)                                                   | 48-51
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Avg (float)                                                   | 52-55
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | RMS (float)                                                   | 56-59
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
@

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

  @float@:  big-endian IEEE 754-2008 binary32 (IEEE 754-1985 single)
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
-- Float

instance ZoomReadable Float where
    data SummaryData Float = SummaryFloat
        { summaryFloatEntry :: {-# UNPACK #-}!Float
        , summaryFloatExit  :: {-# UNPACK #-}!Float
        , summaryFloatMin   :: {-# UNPACK #-}!Float
        , summaryFloatMax   :: {-# UNPACK #-}!Float
        , summaryFloatAvg   :: {-# UNPACK #-}!Double
        , summaryFloatRMS   :: {-# UNPACK #-}!Double
        }

    trackIdentifier = const "ZOOMf32b"

    readRaw     = readFloat32be
    readSummary = readSummaryNum

    prettyRaw         = prettyPacketFloat
    prettySummaryData = prettySummaryFloat

{-# SPECIALIZE readSummaryNum :: (Functor m, MonadIO m) => Iteratee [Word8] m (SummaryData Float) #-}
{-# SPECIALIZE readSummaryNum :: (Functor m, MonadIO m) => Iteratee ByteString m (SummaryData Float) #-}

instance ZoomWrite Float where
    write = writeData

instance ZoomWrite (TimeStamp, Float) where
    write = writeDataVBR

instance ZoomWritable Float where
    data SummaryWork Float = SummaryWorkFloat
        { swFloatTime  :: {-# UNPACK #-}!TimeStamp
        , swFloatEntry :: !(Maybe Float)
        , swFloatExit  :: {-# UNPACK #-}!Float
        , swFloatMin   :: {-# UNPACK #-}!Float
        , swFloatMax   :: {-# UNPACK #-}!Float
        , swFloatSum   :: {-# UNPACK #-}!Double
        , swFloatSumSq :: {-# UNPACK #-}!Double
        }
    fromRaw           = fromFloat
    fromSummaryData   = fromSummaryNum

    initSummaryWork   = initSummaryFloat
    toSummaryData     = mkSummaryNum
    updateSummaryData = updateSummaryNum
    appendSummaryData = appendSummaryNum

instance ZoomNum Float where
    numEntry = summaryFloatEntry
    numExit  = summaryFloatExit
    numMin   = summaryFloatMin
    numMax   = summaryFloatMax
    numAvg   = summaryFloatAvg
    numRMS   = summaryFloatRMS

    numWorkTime  = swFloatTime
    numWorkEntry = swFloatEntry
    numWorkExit  = swFloatExit
    numWorkMin   = swFloatMin
    numWorkMax   = swFloatMax
    numWorkSum   = swFloatSum
    numWorkSumSq = swFloatSumSq

    numMkSummary = SummaryFloat
    numMkSummaryWork = SummaryWorkFloat

{-# SPECIALIZE fromSummaryNum :: SummaryData Float -> Builder #-}
{-# SPECIALIZE mkSummaryNum :: TimeStampDiff -> SummaryWork Float -> SummaryData Float #-}
{-# SPECIALIZE appendSummaryNum :: TimeStampDiff -> SummaryData Float -> TimeStampDiff -> SummaryData Float -> SummaryData Float #-}
{-# SPECIALIZE updateSummaryNum :: TimeStamp -> Float -> SummaryWork Float -> SummaryWork Float #-}

----------------------------------------------------------------------
-- Double

instance ZoomReadable Double where
    data SummaryData Double = SummaryDouble
        { summaryDoubleEntry :: {-# UNPACK #-}!Double
        , summaryDoubleExit  :: {-# UNPACK #-}!Double
        , summaryDoubleMin   :: {-# UNPACK #-}!Double
        , summaryDoubleMax   :: {-# UNPACK #-}!Double
        , summaryDoubleAvg   :: {-# UNPACK #-}!Double
        , summaryDoubleRMS   :: {-# UNPACK #-}!Double
        }

    trackIdentifier = const "ZOOMf64b"

    readRaw     = readDouble64be
    readSummary = readSummaryNum

    prettyRaw         = prettyPacketFloat
    prettySummaryData = prettySummaryFloat

{-# SPECIALIZE readSummaryNum :: (Functor m, MonadIO m) => Iteratee [Word8] m (SummaryData Double) #-}
{-# SPECIALIZE readSummaryNum :: (Functor m, MonadIO m) => Iteratee ByteString m (SummaryData Double) #-}

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

    initSummaryWork   = initSummaryFloat
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

----------------------------------------------------------------------

prettyPacketFloat :: PrintfArg a => a -> String
prettyPacketFloat = printf "%.3f"

prettySummaryFloat :: (PrintfArg a, ZoomNum a)
                   => SummaryData a -> String
prettySummaryFloat s = concat
    [ printf "\tentry: %.3f\texit: %.3f\tmin: %.3f\tmax: %.3f\t"
          (numEntry s) (numExit s) (numMin s) (numMax s)
    , printf "avg: %.3f\trms: %.3f" (numAvg s) (numRMS s)
    ]

initSummaryFloat :: (RealFloat a, ZoomNum a)
                 => TimeStamp -> SummaryWork a
initSummaryFloat entry = numMkSummaryWork
    entry
    Nothing
    0.0
    floatMax
    (negate floatMax)
    0.0
    0.0

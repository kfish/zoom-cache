{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
{- |
   Module      : Data.ZoomCache.Numeric.Int
   Copyright   : Conrad Parker
   License     : BSD3-style (see LICENSE)

   Maintainer  : Conrad Parker <conrad@metadecks.org>
   Stability   : unstable
   Portability : unknown

Default codec implementation for values of type Int. This module
implements the interfaces documented in "Data.ZoomCache.Codec".
View the module source for enlightenment.

The table below describes the encoding of SummaryData for Int and Int32:

@
   | ...                                                           |   -35
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Entry (int32)                                                 | 36-39
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Exit (int32)                                                  | 40-43
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Min (int32)                                                   | 44-47
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Max (int32)                                                   | 48-51
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Avg (double)                                                  | 52-55
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 56-59
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | RMS (double)                                                  | 60-63
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 64-67
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
@

Field encoding formats:

  @int32@:  32bit big endian

  @double@: big-endian IEEE 754-2008 binary64 (IEEE 754-1985 double)

-}
----------------------------------------------------------------------

module Data.ZoomCache.Numeric.Int (
      SummaryData(..)
    , SummaryWork(..)
)where

import Blaze.ByteString.Builder
import Control.Monad.Trans (MonadIO)
import Data.ByteString (ByteString)
import Data.Int
import Data.Iteratee (Iteratee)
import Data.Word
import Text.Printf

import Data.ZoomCache.Codec
import Data.ZoomCache.Numeric.Internal
import Data.ZoomCache.Numeric.Types

----------------------------------------------------------------------
-- Int

instance ZoomReadable Int where
    data SummaryData Int = SummaryInt
        { summaryIntEntry :: {-# UNPACK #-}!Int
        , summaryIntExit  :: {-# UNPACK #-}!Int
        , summaryIntMin   :: {-# UNPACK #-}!Int
        , summaryIntMax   :: {-# UNPACK #-}!Int
        , summaryIntAvg   :: {-# UNPACK #-}!Double
        , summaryIntRMS   :: {-# UNPACK #-}!Double
        }

    trackIdentifier = const "ZOOMi32b"

    readRaw     = readInt32be
    readSummary = readSummaryNum

    prettyRaw         = show
    prettySummaryData = prettySummaryInt

{-# SPECIALIZE readSummaryNum :: (Functor m, MonadIO m) => Iteratee [Word8] m (SummaryData Int) #-}
{-# SPECIALIZE readSummaryNum :: (Functor m, MonadIO m) => Iteratee ByteString m (SummaryData Int) #-}

instance ZoomWrite Int where
    write = writeData

instance ZoomWrite (TimeStamp, Int) where
    write = writeDataVBR

instance ZoomWritable Int where
    data SummaryWork Int = SummaryWorkInt
        { swIntTime  :: {-# UNPACK #-}!TimeStamp
        , swIntEntry :: !(Maybe Int)
        , swIntExit  :: {-# UNPACK #-}!Int
        , swIntMin   :: {-# UNPACK #-}!Int
        , swIntMax   :: {-# UNPACK #-}!Int
        , swIntSum   :: {-# UNPACK #-}!Double
        , swIntSumSq :: {-# UNPACK #-}!Double
        }

    fromRaw           = fromIntegral32be
    fromSummaryData   = fromSummaryNum

    initSummaryWork   = initSummaryNumBounded
    toSummaryData     = mkSummaryNum
    updateSummaryData = updateSummaryNum
    appendSummaryData = appendSummaryNum

instance ZoomNum Int where
    numEntry = summaryIntEntry
    numExit = summaryIntExit
    numMin = summaryIntMin
    numMax = summaryIntMax
    numAvg = summaryIntAvg
    numRMS = summaryIntRMS

    numWorkTime = swIntTime
    numWorkEntry = swIntEntry
    numWorkExit = swIntExit
    numWorkMin = swIntMin
    numWorkMax = swIntMax
    numWorkSum = swIntSum
    numWorkSumSq = swIntSumSq

    numMkSummary = SummaryInt
    numMkSummaryWork = SummaryWorkInt

{-# SPECIALIZE fromSummaryNum :: SummaryData Int -> Builder #-}
{-# SPECIALIZE initSummaryNumBounded :: TimeStamp -> SummaryWork Int #-}
{-# SPECIALIZE mkSummaryNum :: TimeStampDiff -> SummaryWork Int -> SummaryData Int #-}
{-# SPECIALIZE appendSummaryNum :: TimeStampDiff -> SummaryData Int -> TimeStampDiff -> SummaryData Int -> SummaryData Int #-}
{-# SPECIALIZE updateSummaryNum :: TimeStamp -> Int -> SummaryWork Int -> SummaryWork Int #-}

----------------------------------------------------------------------
-- Int32

instance ZoomReadable Int32 where
    data SummaryData Int32 = SummaryInt32
        { summaryInt32Entry :: {-# UNPACK #-}!Int32
        , summaryInt32Exit  :: {-# UNPACK #-}!Int32
        , summaryInt32Min   :: {-# UNPACK #-}!Int32
        , summaryInt32Max   :: {-# UNPACK #-}!Int32
        , summaryInt32Avg   :: {-# UNPACK #-}!Double
        , summaryInt32RMS   :: {-# UNPACK #-}!Double
        }

    trackIdentifier = const "ZOOMi32b"

    readRaw     = readInt32be
    readSummary = readSummaryNum

    prettyRaw         = show
    prettySummaryData = prettySummaryInt

{-# SPECIALIZE readSummaryNum :: (Functor m, MonadIO m) => Iteratee [Word8] m (SummaryData Int32) #-}
{-# SPECIALIZE readSummaryNum :: (Functor m, MonadIO m) => Iteratee ByteString m (SummaryData Int32) #-}

instance ZoomWrite Int32 where
    write = writeData

instance ZoomWrite (TimeStamp, Int32) where
    write = writeDataVBR

instance ZoomWritable Int32 where
    data SummaryWork Int32 = SummaryWorkInt32
        { swInt32Time  :: {-# UNPACK #-}!TimeStamp
        , swInt32Entry :: !(Maybe Int32)
        , swInt32Exit  :: {-# UNPACK #-}!Int32
        , swInt32Min   :: {-# UNPACK #-}!Int32
        , swInt32Max   :: {-# UNPACK #-}!Int32
        , swInt32Sum   :: {-# UNPACK #-}!Double
        , swInt32SumSq :: {-# UNPACK #-}!Double
        }

    fromRaw           = fromIntegral32be
    fromSummaryData   = fromSummaryNum

    initSummaryWork   = initSummaryNumBounded
    toSummaryData     = mkSummaryNum
    updateSummaryData = updateSummaryNum
    appendSummaryData = appendSummaryNum

instance ZoomNum Int32 where
    numEntry = summaryInt32Entry
    numExit = summaryInt32Exit
    numMin = summaryInt32Min
    numMax = summaryInt32Max
    numAvg = summaryInt32Avg
    numRMS = summaryInt32RMS

    numWorkTime = swInt32Time
    numWorkEntry = swInt32Entry
    numWorkExit = swInt32Exit
    numWorkMin = swInt32Min
    numWorkMax = swInt32Max
    numWorkSum = swInt32Sum
    numWorkSumSq = swInt32SumSq

    numMkSummary = SummaryInt32
    numMkSummaryWork = SummaryWorkInt32

{-# SPECIALIZE fromSummaryNum :: SummaryData Int32 -> Builder #-}
{-# SPECIALIZE initSummaryNumBounded :: TimeStamp -> SummaryWork Int32 #-}
{-# SPECIALIZE mkSummaryNum :: TimeStampDiff -> SummaryWork Int32 -> SummaryData Int32 #-}
{-# SPECIALIZE appendSummaryNum :: TimeStampDiff -> SummaryData Int32 -> TimeStampDiff -> SummaryData Int32 -> SummaryData Int32 #-}
{-# SPECIALIZE updateSummaryNum :: TimeStamp -> Int32 -> SummaryWork Int32 -> SummaryWork Int32 #-}

----------------------------------------------------------------------

prettySummaryInt :: (PrintfArg a, ZoomNum a)
                 => SummaryData a -> String
prettySummaryInt s = concat
    [ printf "\tentry: %d\texit: %df\tmin: %d\tmax: %d\t"
          (numEntry s) (numExit s) (numMin s) (numMax s)
    , printf "avg: %.3f\trms: %.3f" (numAvg s) (numRMS s)
    ]


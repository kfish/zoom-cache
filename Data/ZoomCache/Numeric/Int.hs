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

The table below describes the encoding of SummaryData for Int.

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
import Control.Monad (replicateM)
import Control.Monad.Trans (MonadIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Iteratee (Iteratee)
import qualified Data.Iteratee as I
import qualified Data.ListLike as LL
import Data.Word
import Text.Printf

import Data.ZoomCache.Codec
import Data.ZoomCache.Numeric.Internal
import Data.ZoomCache.Numeric.Types

----------------------------------------------------------------------

-- Identifier for track headers
trackTypeInt :: ByteString
trackTypeInt = "ZOOMi32b"

----------------------------------------------------------------------
-- Read

instance ZoomReadable Int where
    data SummaryData Int = SummaryInt
        { summaryIntEntry :: {-# UNPACK #-}!Int
        , summaryIntExit  :: {-# UNPACK #-}!Int
        , summaryIntMin   :: {-# UNPACK #-}!Int
        , summaryIntMax   :: {-# UNPACK #-}!Int
        , summaryIntAvg   :: {-# UNPACK #-}!Double
        , summaryIntRMS   :: {-# UNPACK #-}!Double
        }

    trackIdentifier = const trackTypeInt

    readRaw     = readInt32be
    readSummary = readSummaryInt

    prettyRaw         = prettyPacketInt
    prettySummaryData = prettySummaryInt

prettyPacketInt :: Int -> String
prettyPacketInt = show

readSummaryInt :: (I.Nullable s, LL.ListLike s Word8, Functor m, MonadIO m)
               => Iteratee s m (SummaryData Int)
readSummaryInt = do
    [en,ex,mn,mx] <- replicateM 4 readInt32be
    [avg,rms] <- replicateM 2 readDouble64be
    return (SummaryInt en ex mn mx avg rms)
{-# SPECIALIZE INLINE readSummaryInt :: (Functor m, MonadIO m) => Iteratee [Word8] m (SummaryData Int) #-}
{-# SPECIALIZE INLINE readSummaryInt :: (Functor m, MonadIO m) => Iteratee B.ByteString m (SummaryData Int) #-}

prettySummaryInt :: SummaryData Int -> String
prettySummaryInt SummaryInt{..} = concat
    [ printf "\tentry: %d\texit: %df\tmin: %d\tmax: %d\t"
          summaryIntEntry summaryIntExit summaryIntMin summaryIntMax
    , printf "avg: %.3f\trms: %.3f" summaryIntAvg summaryIntRMS
    ]

----------------------------------------------------------------------
-- Write

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

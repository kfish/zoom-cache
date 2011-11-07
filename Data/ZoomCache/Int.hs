{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
{- |
   Module      : Data.ZoomCache.Int
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

module Data.ZoomCache.Int (
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

{-
    typeOfSummaryData = typeOfSummaryInt

typeOfSummaryInt :: SummaryData Int -> TypeRep
typeOfSummaryInt _ = mkTyConApp tyCon [i,i,i,i]
    where
        tyCon = mkTyCon3 "zoom-cache" "Data.ZoomCache.Types" "SummaryInt"
        i = typeOf (undefined :: Int)
-}

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
        , swIntSum   :: {-# UNPACK #-}!Int
        , swIntSumSq :: {-# UNPACK #-}!Double
        }

    fromRaw           = fromIntegral32be
    fromSummaryData   = fromSummaryInt

    initSummaryWork   = initSummaryInt
    toSummaryData     = mkSummaryInt
    updateSummaryData = updateSummaryInt
    appendSummaryData = appendSummaryInt

initSummaryInt :: TimeStamp -> SummaryWork Int
initSummaryInt entry = SummaryWorkInt
    { swIntTime = entry
    , swIntEntry = Nothing
    , swIntExit = 0
    , swIntMin = maxBound
    , swIntMax = minBound
    , swIntSum = 0
    , swIntSumSq = 0
    }

mkSummaryInt :: TimeStampDiff -> SummaryWork Int -> SummaryData Int
mkSummaryInt dur SummaryWorkInt{..} = SummaryInt
    { summaryIntEntry = fromMaybe 0 swIntEntry
    , summaryIntExit = swIntExit
    , summaryIntMin = swIntMin
    , summaryIntMax = swIntMax
    , summaryIntAvg = fromIntegral swIntSum / fromIntegral (unTSDiff dur)
    , summaryIntRMS = sqrt $ swIntSumSq / fromIntegral (unTSDiff dur)
    }

fromSummaryInt :: SummaryData Int -> Builder
fromSummaryInt SummaryInt{..} = mconcat $ map fromIntegral32be
    [ summaryIntEntry
    , summaryIntExit
    , summaryIntMin
    , summaryIntMax
    ] ++ map fromDouble
    [ summaryIntAvg
    , summaryIntRMS
    ]

updateSummaryInt :: TimeStamp  -> Int -> SummaryWork Int
                 -> SummaryWork Int
updateSummaryInt t i SummaryWorkInt{..} = SummaryWorkInt
    { swIntTime = t
    , swIntEntry = Just $ fromMaybe i swIntEntry
    , swIntExit = i
    , swIntMin = min swIntMin i
    , swIntMax = max swIntMax i
    , swIntSum = swIntSum + (i * dur)
    , swIntSumSq = swIntSumSq + fromIntegral (i*i * dur)
    }
    where
        !dur = fromIntegral $ (unTS t) - (unTS swIntTime)

appendSummaryInt :: TimeStampDiff -> SummaryData Int
                 -> TimeStampDiff -> SummaryData Int
                 -> SummaryData Int
appendSummaryInt (TSDiff dur1) s1 (TSDiff dur2) s2 = SummaryInt
    { summaryIntEntry = summaryIntEntry s1
    , summaryIntExit = summaryIntExit s2
    , summaryIntMin = min (summaryIntMin s1) (summaryIntMin s2)
    , summaryIntMax = max (summaryIntMax s1) (summaryIntMax s2)
    , summaryIntAvg = ((summaryIntAvg s1 * fromIntegral dur1) +
                       (summaryIntAvg s2 * fromIntegral dur2)) /
                      fromIntegral durSum
    , summaryIntRMS = sqrt $ ((summaryIntRMS s1 * summaryIntRMS s1 *
                               fromIntegral dur1) +
                              (summaryIntRMS s2 * summaryIntRMS s2 *
                               fromIntegral dur2)) /
                             fromIntegral durSum
    }
    where
        !durSum = dur1 + dur2


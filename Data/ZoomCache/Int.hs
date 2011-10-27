{-# LANGUAGE FlexibleInstances #-}
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
      RawData(..)
    , SummaryData(..)
    , SummaryWork(..)
)where

import Blaze.ByteString.Builder
import Control.Monad (replicateM)
import Control.Monad.Trans (MonadIO)
import Data.Iteratee (Iteratee)
import Data.Monoid
import Data.Word
import Text.Printf

import Data.ZoomCache.Codec

----------------------------------------------------------------------
-- Read

instance ZoomReadable Int where
    data RawData Int = RDInt [Int]

    readRaw  = readInt32be
    fromList = RDInt

    data SummaryData Int = SummaryInt
        { summaryIntEntry :: Int
        , summaryIntExit  :: Int
        , summaryIntMin   :: Int
        , summaryIntMax   :: Int
        , summaryIntAvg   :: Double
        , summaryIntRMS   :: Double
        }

    readSummary = readSummaryInt

    prettyRawData  = prettyPacketInt
    prettySummaryData = prettySummaryInt

prettyPacketInt :: RawData Int -> [String]
prettyPacketInt (RDInt ds) = map show ds

readSummaryInt :: (Functor m, MonadIO m)
               => Iteratee [Word8] m (SummaryData Int)
readSummaryInt = do
    [en,ex,mn,mx] <- replicateM 4 readInt32be
    [avg,rms] <- replicateM 2 readDouble64be
    return (SummaryInt en ex mn mx avg rms)

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
        { ztsiTime  :: TimeStamp
        , ztsiEntry :: Int
        , ztsiExit  :: Int
        , ztsiMin   :: Int
        , ztsiMax   :: Int
        , ztsiSum   :: Int
        , ztsiSumSq :: Double
        }

    fromRaw           = fromIntegral32be
    fromSummaryData   = fromSummaryInt

    initSummaryWork   = initSummaryInt
    toSummaryData     = mkSummaryInt
    updateSummaryData = updateSummaryInt
    appendSummaryData = appendSummaryInt

initSummaryInt :: TimeStamp -> SummaryWork Int
initSummaryInt entry = SummaryWorkInt
    { ztsiTime = entry
    , ztsiEntry = 0
    , ztsiExit = 0
    , ztsiMin = maxBound
    , ztsiMax = minBound
    , ztsiSum = 0
    , ztsiSumSq = 0
    }

mkSummaryInt :: Double -> SummaryWork Int -> SummaryData Int
mkSummaryInt dur SummaryWorkInt{..} = SummaryInt
    { summaryIntEntry = ztsiEntry
    , summaryIntExit = ztsiExit
    , summaryIntMin = ztsiMin
    , summaryIntMax = ztsiMax
    , summaryIntAvg = fromIntegral ztsiSum / dur
    , summaryIntRMS = sqrt $ ztsiSumSq / dur
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

updateSummaryInt :: Int -> TimeStamp  -> Int -> SummaryWork Int
                 -> SummaryWork Int
updateSummaryInt count t i SummaryWorkInt{..} = SummaryWorkInt
    { ztsiTime = t
    , ztsiEntry = if count == 0 then i else ztsiEntry
    , ztsiExit = i
    , ztsiMin = min ztsiMin i
    , ztsiMax = max ztsiMax i
    , ztsiSum = ztsiSum + (i * dur)
    , ztsiSumSq = ztsiSumSq + fromIntegral (i*i * dur)
    }
    where
        dur = fromIntegral $ (unTS t) - (unTS ztsiTime)

appendSummaryInt :: Double -> SummaryData Int
                 -> Double -> SummaryData Int
                 -> SummaryData Int
appendSummaryInt dur1 s1 dur2 s2 = SummaryInt
    { summaryIntEntry = summaryIntEntry s1
    , summaryIntExit = summaryIntExit s2
    , summaryIntMin = min (summaryIntMin s1) (summaryIntMin s2)
    , summaryIntMax = max (summaryIntMax s1) (summaryIntMax s2)
    , summaryIntAvg = ((summaryIntAvg s1 * dur1) +
                       (summaryIntAvg s2 * dur2)) /
                      durSum
    , summaryIntRMS = sqrt $ ((summaryIntRMS s1 * summaryIntRMS s1 * dur1) +
                              (summaryIntRMS s2 * summaryIntRMS s2 * dur2)) /
                             durSum
    }
    where
        durSum = dur1 + dur2


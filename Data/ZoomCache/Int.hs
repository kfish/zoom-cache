{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
-- |
-- Module      : Data.ZoomCache.Int
-- Copyright   : Conrad Parker
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Conrad Parker <conrad@metadecks.org>
-- Stability   : unstable
-- Portability : unknown
--
-- Default codec implementation for values of type Int. This module
-- implements the interfaces documented in "Data.ZoomCache.Codec".
-- View the module source for enlightenment.
----------------------------------------------------------------------

module Data.ZoomCache.Int (
      PacketData(..)
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

instance ZoomRead Int where
    data PacketData Int = PDInt [Int]
    zRead = zReadInt32
    packetDataFromList = PDInt
    prettyPacketData = prettyPacketInt

prettyPacketInt :: PacketData Int -> [String]
prettyPacketInt (PDInt ds) = map show ds

----------------------------------------------------------------------
-- Write

instance ZoomWrite Int where
    write = writeData

instance ZoomWrite (TimeStamp, Int) where
    write = writeDataVBR

----------------------------------------------------------------------
-- Summary

instance ZoomSummary Int where
    data SummaryData Int = SummaryInt
        { summaryIntEntry :: Int
        , summaryIntExit  :: Int
        , summaryIntMin   :: Int
        , summaryIntMax   :: Int
        , summaryIntAvg   :: Double
        , summaryIntRMS   :: Double
        }
    readSummaryData = readSummaryDataInt
    prettySummaryData = prettySummaryInt

readSummaryDataInt :: (Functor m, MonadIO m)
                   => Iteratee [Word8] m (SummaryData Int)
readSummaryDataInt = do
    [en,ex,mn,mx] <- replicateM 4 zReadInt32
    [avg,rms] <- replicateM 2 zReadFloat64be
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

instance ZoomSummaryWrite Int where
    data SummaryWork Int = SummaryWorkInt
        { ztsiTime  :: TimeStamp
        , ztsiEntry :: Int
        , ztsiExit  :: Int
        , ztsiMin   :: Int
        , ztsiMax   :: Int
        , ztsiSum   :: Int
        , ztsiSumSq :: Double
        }
    builder           = fromIntegral32be
    initSummaryWork   = initSummaryInt
    mkSummaryData     = mkSummaryInt
    fromSummaryData   = fromSummaryInt
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


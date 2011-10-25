{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Data.ZoomCache.Double (
      SummaryData(..)
    , SummaryWork(..)
)where

import Blaze.ByteString.Builder
import Data.Monoid
import Text.Printf

import Data.ZoomCache.Common
import Data.ZoomCache.Types
import Data.ZoomCache.Write
import Numeric.FloatMinMax

----------------------------------------------------------------------
-- Read

instance ZoomRead Double where
    data PacketData Double = PDDouble [Double]

----------------------------------------------------------------------
-- Write

instance ZoomWrite Double where
    write = writeData

instance ZoomWrite (TimeStamp, Double) where
    write = writeDataVBR

----------------------------------------------------------------------
-- Summary

instance ZoomSummary Double where
    data SummaryData Double = SummaryDouble
        { summaryDoubleEntry :: Double
        , summaryDoubleExit  :: Double
        , summaryDoubleMin   :: Double
        , summaryDoubleMax   :: Double
        , summaryDoubleAvg   :: Double
        , summaryDoubleRMS   :: Double
        }
    prettySummaryData = prettySummaryDouble

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

instance ZoomSummaryWrite Double where
    data SummaryWork Double = SummaryWorkDouble
        { ztsdTime  :: TimeStamp
        , ztsdEntry :: Double
        , ztsdExit  :: Double
        , ztsdMin   :: Double
        , ztsdMax   :: Double
        , ztsdSum   :: Double
        , ztsdSumSq :: Double
        }
    builder           = fromWord64be . toWord64
    initSummaryWork   = initSummaryDouble
    mkSummaryData     = mkSummaryDouble
    fromSummaryData   = fromSummaryDouble
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
fromSummaryDouble SummaryDouble{..} = mconcat $ map encDbl
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
        dur = fromIntegral $ (unTS t) - (unTS ztsdTime)

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
        durSum = dur1 + dur2


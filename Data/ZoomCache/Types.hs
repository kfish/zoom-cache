{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      : Data.ZoomCache.Types
-- Copyright   : Conrad Parker
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Conrad Parker <conrad@metadecks.org>
-- Stability   : unstable
-- Portability : unknown
--
-- ZoomCache packet definition
----------------------------------------------------------------------

module Data.ZoomCache.Types (
    -- * Classes
      ZoomRead(..)
    , PacketData(..)
    , ZoomSummary(..)
    , ZoomSummaryWrite(..)

    -- * Types
    , Packet(..)
    , Summary(..)
    , SummaryData(..)
    , SummaryWork(..)

    , summaryDuration
    , appendSummary

    -- * Builder helpers
    , encInt
    , encInt64
    , encDbl
    , fromRational64
    , toWord64
) where

import Blaze.ByteString.Builder
import Data.Dynamic
import Data.Monoid
import Data.Typeable
import Text.Printf

-- Binary helpers
import Data.Ratio
import Data.Word
import Unsafe.Coerce (unsafeCoerce)

import Data.ZoomCache.Common
import Numeric.FloatMinMax

------------------------------------------------------------

class ZoomRead a where
    data PacketData a :: *

instance ZoomRead Dynamic where
    data PacketData Dynamic = PDDynamic [Dynamic]

instance ZoomRead Double where
    data PacketData Double = PDDouble [Double]

instance ZoomRead Int where
    data PacketData Int = PDInt [Int]

------------------------------------------------------------

data Packet a = Packet
    { packetTrack      :: TrackNo
    , packetEntryTime  :: TimeStamp
    , packetExitTime   :: TimeStamp
    , packetCount      :: Int
    , packetData       :: PacketData a
    , packetTimeStamps :: [TimeStamp]
    }

------------------------------------------------------------

class ZoomSummary a where
    data SummaryData a :: *
    prettySummaryData  :: SummaryData a -> String
    -- typeOfSummaryData :: SummaryData a -> TypeRep

instance Typeable1 SummaryData

class ZoomSummary a => ZoomSummaryWrite a where
    data SummaryWork a :: *
    initSummaryWork    :: TimeStamp -> SummaryWork a
    mkSummaryData      :: Double -> SummaryWork a -> SummaryData a
    fromSummaryData    :: SummaryData a -> Builder
    updateSummaryData  :: Int -> TimeStamp -> a
                       -> SummaryWork a
                       -> SummaryWork a
    appendSummaryData  :: Double -> SummaryData a
                       -> Double -> SummaryData a
                       -> SummaryData a

------------------------------------------------------------
-- Dynamic

instance ZoomSummary Dynamic where
    data SummaryData Dynamic = SummaryDynamic Dynamic
    prettySummaryData = prettySummaryDynamic
    -- typeOfSummaryData = const (typeOf (undefined :: Dynamic))

prettySummaryDynamic :: SummaryData Dynamic -> String
prettySummaryDynamic _ = "<<SummaryDynamic>>"

------------------------------------------------------------
-- Double

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

------------------------------------------------------------
-- Int

instance ZoomSummary Int where
    data SummaryData Int = SummaryInt
        { summaryIntEntry :: Int
        , summaryIntExit  :: Int
        , summaryIntMin   :: Int
        , summaryIntMax   :: Int
        , summaryIntAvg   :: Double
        , summaryIntRMS   :: Double
        }
    prettySummaryData = prettySummaryInt

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
fromSummaryInt SummaryInt{..} = mconcat $ map encInt
    [ summaryIntEntry
    , summaryIntExit
    , summaryIntMin
    , summaryIntMax
    ] ++ map encDbl
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

------------------------------------------------------------
-- | A recorded block of summary data
data Summary a = Summary
    { summaryTrack :: TrackNo
    , summaryLevel :: Int
    , summaryEntryTime :: TimeStamp
    , summaryExitTime :: TimeStamp
    , summaryData :: SummaryData a
    }

-- | The duration covered by a summary, in units of 1 / the track's datarate
summaryDuration :: Summary a -> Integer
summaryDuration s = (unTS $ summaryExitTime s) - (unTS $ summaryEntryTime s)

-- | Append two Summaries, merging statistical summary data.
-- XXX: summaries are only compatible if tracks and levels are equal
appendSummary :: (ZoomSummaryWrite a) => Summary a -> Summary a -> Summary a
appendSummary s1 s2 = Summary
    { summaryTrack = summaryTrack s1
    , summaryLevel = summaryLevel s1
    , summaryEntryTime = summaryEntryTime s1
    , summaryExitTime = summaryExitTime s2
    , summaryData = appendSummaryData (dur s1) (summaryData s1)
                                      (dur s2) (summaryData s2)
    }
    where
        dur = fromIntegral . summaryDuration

----------------------------------------------------------------------
-- Binary data helpers
    
fromRational64 :: Rational -> Builder
fromRational64 r = mconcat
    [ fromInt64be . fromIntegral . numerator $ r
    , fromInt64be . fromIntegral . denominator $ r
    ]

encInt :: forall a . (Integral a) => a -> Builder
encInt = fromInt32be . fromIntegral

encInt64 :: forall a . (Integral a) => a -> Builder
encInt64 = fromInt64be . fromIntegral

encDbl :: Double -> Builder
encDbl = fromWord64be . toWord64

toWord64 :: Double -> Word64
toWord64 = unsafeCoerce


{-# LANGUAGE ExistentialQuantification #-}
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
    , PacketData()
    , ZoomSummary(..)
    , ZoomSummaryWrite(..)

    , OpaqueSummary(..)
    , mkOpaqueSummary

    , OpaqueSummaryWrite(..)
    , clearWork
    , clearLevel
    , updateOpSumm

    -- * Types
    , Packet(..)
    , Summary(..)
    , SummaryData()

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
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Monoid

-- Binary helpers
import Data.Ratio
import Data.Word
import Unsafe.Coerce (unsafeCoerce)

import Data.ZoomCache.Common

------------------------------------------------------------

class ZoomRead a where
    data PacketData a :: *

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

data OpaqueSummary = forall a . ZoomSummary a => OpSummary (Summary a)

mkOpaqueSummary :: ZoomSummary a => Summary a -> OpaqueSummary
mkOpaqueSummary = OpSummary

data OpaqueSummaryWrite = forall a . (Typeable a, ZoomSummaryWrite a) => OpSummaryWrite
    { levels   :: IntMap (Summary a -> Summary a)
    , currWork :: Maybe (SummaryWork a)
    }

clearWork :: OpaqueSummaryWrite -> OpaqueSummaryWrite
clearWork (OpSummaryWrite l _) = OpSummaryWrite l Nothing

clearLevel :: Int -> OpaqueSummaryWrite -> OpaqueSummaryWrite
clearLevel level (OpSummaryWrite l cw) = OpSummaryWrite (IM.delete level l) cw

updateOpSumm :: (Typeable b, ZoomSummaryWrite b)
             => Int -> TimeStamp -> b
             -> Maybe OpaqueSummaryWrite
             -> Maybe OpaqueSummaryWrite

updateOpSumm count t d Nothing = Just (OpSummaryWrite IM.empty (Just cw))
    where
        cw = updateSummaryData count t d (initSummaryWork t)

updateOpSumm count t d (Just (OpSummaryWrite l Nothing)) =
    case cw'm of
        Just _  -> Just (OpSummaryWrite l cw'm)
        Nothing -> Nothing
    where
        cw'm = case (fromDynamic . toDyn $ d) of
            Just d' -> Just (updateSummaryData count t d' (initSummaryWork t))
            Nothing -> Nothing

updateOpSumm count t d (Just (OpSummaryWrite l (Just cw))) =
    case cw'm of
        Just _  -> Just (OpSummaryWrite l cw'm)
        Nothing -> Nothing
    where
        cw'm = case (fromDynamic . toDyn $ d) of
            Just d' -> Just (updateSummaryData count t d' cw)
            Nothing -> Nothing

------------------------------------------------------------

class ZoomSummary a where
    data SummaryData a :: *
    prettySummaryData  :: SummaryData a -> String
    -- typeOfSummaryData :: SummaryData a -> TypeRep

class ZoomSummary a => ZoomSummaryWrite a where
    data SummaryWork a :: *
    builder            :: a -> Builder
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


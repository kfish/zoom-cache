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
    , RawData()
    , ZoomSummaryWrite(..)

    , ZoomRaw(..)
    , mkOpaquePacketData

    , OpaqueSummary(..)

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
) where

import Blaze.ByteString.Builder
import Control.Monad.Trans (MonadIO)
import Data.Dynamic
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Iteratee (Iteratee)
import Data.Word

import Data.ZoomCache.Common

------------------------------------------------------------

data Packet = Packet
    { packetTrack      :: TrackNo
    , packetEntryTime  :: TimeStamp
    , packetExitTime   :: TimeStamp
    , packetCount      :: Int
    , packetData       :: ZoomRaw
    , packetTimeStamps :: [TimeStamp]
    }

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

------------------------------------------------------------
-- Read

class ZoomRead a where
    data RawData a  :: *
    readRaw         :: (Functor m, MonadIO m)
                    => Iteratee [Word8] m a
    fromList        :: [a] -> RawData a

    data SummaryData a :: *
    readSummary        :: (Functor m, MonadIO m)
                       => Iteratee [Word8] m (SummaryData a)

    prettyRawData      :: RawData a -> [String]
    prettySummaryData  :: SummaryData a -> String
    -- typeOfSummaryData :: SummaryData a -> TypeRep

data ZoomRaw = forall a . ZoomRead a => ZoomRaw (RawData a)

mkOpaquePacketData :: ZoomRead a => [a] -> ZoomRaw
mkOpaquePacketData = ZoomRaw . fromList

data OpaqueSummary = forall a . ZoomRead a => OpSummary (Summary a)

------------------------------------------------------------
-- Write

class ZoomSummaryWrite a where
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


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
      ZoomReadable(..)
    , RawData()
    , ZoomWritable(..)

    , ZoomRaw(..)

    , ZoomSummary(..)

    , ZoomWork(..)
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
appendSummary :: (ZoomWritable a) => Summary a -> Summary a -> Summary a
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

class ZoomReadable a where
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

data ZoomRaw = forall a . ZoomReadable a => ZoomRaw (RawData a)

data ZoomSummary = forall a . ZoomReadable a => ZoomSummary (Summary a)

------------------------------------------------------------
-- Write

class ZoomWritable a where
    data SummaryWork a :: *

    fromRaw            :: a -> Builder
    fromSummaryData    :: SummaryData a -> Builder

    initSummaryWork    :: TimeStamp -> SummaryWork a
    toSummaryData      :: Double -> SummaryWork a -> SummaryData a
    updateSummaryData  :: Int -> TimeStamp -> a
                       -> SummaryWork a
                       -> SummaryWork a
    appendSummaryData  :: Double -> SummaryData a
                       -> Double -> SummaryData a
                       -> SummaryData a

data ZoomWork = forall a . (Typeable a, ZoomWritable a) => ZoomWork
    { levels   :: IntMap (Summary a -> Summary a)
    , currWork :: Maybe (SummaryWork a)
    }

clearWork :: ZoomWork -> ZoomWork
clearWork (ZoomWork l _) = ZoomWork l Nothing

clearLevel :: Int -> ZoomWork -> ZoomWork
clearLevel level (ZoomWork l cw) = ZoomWork (IM.delete level l) cw

updateOpSumm :: (Typeable b, ZoomWritable b)
             => Int -> TimeStamp -> b
             -> Maybe ZoomWork
             -> Maybe ZoomWork

updateOpSumm count t d Nothing = Just (ZoomWork IM.empty (Just cw))
    where
        cw = updateSummaryData count t d (initSummaryWork t)

updateOpSumm count t d (Just (ZoomWork l Nothing)) =
    case cw'm of
        Just _  -> Just (ZoomWork l cw'm)
        Nothing -> Nothing
    where
        cw'm = case (fromDynamic . toDyn $ d) of
            Just d' -> Just (updateSummaryData count t d' (initSummaryWork t))
            Nothing -> Nothing

updateOpSumm count t d (Just (ZoomWork l (Just cw))) =
    case cw'm of
        Just _  -> Just (ZoomWork l cw'm)
        Nothing -> Nothing
    where
        cw'm = case (fromDynamic . toDyn $ d) of
            Just d' -> Just (updateSummaryData count t d' cw)
            Nothing -> Nothing


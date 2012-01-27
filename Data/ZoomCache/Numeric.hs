{-# LANGUAGE RecordWildCards #-}
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
-- ZoomCache numeric API
----------------------------------------------------------------------

module Data.ZoomCache.Numeric (
    ZoomNum
  , numEntry
  , numExit
  , numMin
  , numMax
  , numAvg
  , numRMS

  , rawToDouble
  , toSummaryDouble
  , toSummaryUTCDouble

  , wholeTrackSummaryDouble
  , wholeTrackSummaryUTCDouble
  , enumDouble
  , enumUTCDouble
  , enumSummaryDouble
  , enumSummaryUTCDouble

  , module Data.ZoomCache
) where

import Control.Applicative ((<$>))
import Control.Monad.Trans (MonadIO)
import Data.Int
import qualified Data.Iteratee as I
import Data.Maybe
import Data.Time (UTCTime)
import Data.Typeable
import Data.Word

import Data.Offset
import Data.ZoomCache
import Data.ZoomCache.Numeric.Types

----------------------------------------------------------------------

rawToDouble :: ZoomRaw -> [Double]
rawToDouble (ZoomRaw xs) | typeOf xs == typeOf (undefined :: [Double]) =
                              fromMaybe [] (cast xs :: Maybe [Double])
                         | typeOf xs == typeOf (undefined :: [Float]) =
                                         f (cast xs :: Maybe [Float])
                         | typeOf xs == typeOf (undefined :: [Int]) =
                                         f (cast xs :: Maybe [Int])
                         | typeOf xs == typeOf (undefined :: [Int8]) =
                                         f (cast xs :: Maybe [Int8])
                         | typeOf xs == typeOf (undefined :: [Int16]) =
                                         f (cast xs :: Maybe [Int16])
                         | typeOf xs == typeOf (undefined :: [Int32]) =
                                         f (cast xs :: Maybe [Int32])
                         | typeOf xs == typeOf (undefined :: [Int64]) =
                                         f (cast xs :: Maybe [Int64])
                         | typeOf xs == typeOf (undefined :: [Integer]) =
                                         f (cast xs :: Maybe [Integer])
                         | typeOf xs == typeOf (undefined :: [Word]) =
                                         f (cast xs :: Maybe [Word])
                         | typeOf xs == typeOf (undefined :: [Word8]) =
                                         f (cast xs :: Maybe [Word8])
                         | typeOf xs == typeOf (undefined :: [Word16]) =
                                         f (cast xs :: Maybe [Word16])
                         | typeOf xs == typeOf (undefined :: [Word32]) =
                                         f (cast xs :: Maybe [Word32])
                         | typeOf xs == typeOf (undefined :: [Word64]) =
                                         f (cast xs :: Maybe [Word64])
                         | otherwise = []
    where
        f :: Real a => Maybe [a] -> [Double]
        f = maybe [] (map realToFrac)

----------------------------------------------------------------------

-- | Coercion of numeric Summary to type Summary Double.
toSummaryDouble :: Typeable a => Summary a -> Maybe (Summary Double)
toSummaryDouble s | typeOf s == typeOf (undefined :: Summary Double) =
                                id (cast s :: Maybe (Summary Double))
                  | typeOf s == typeOf (undefined :: Summary Float) =
                            sd <$> (cast s :: Maybe (Summary Float))
                  | typeOf s == typeOf (undefined :: Summary Int) =
                            sd <$> (cast s :: Maybe (Summary Int))
                  | typeOf s == typeOf (undefined :: Summary Int8) =
                            sd <$> (cast s :: Maybe (Summary Int8))
                  | typeOf s == typeOf (undefined :: Summary Int16) =
                            sd <$> (cast s :: Maybe (Summary Int16))
                  | typeOf s == typeOf (undefined :: Summary Int32) =
                            sd <$> (cast s :: Maybe (Summary Int32))
                  | typeOf s == typeOf (undefined :: Summary Int64) =
                            sd <$> (cast s :: Maybe (Summary Int64))
                  | typeOf s == typeOf (undefined :: Summary Integer) =
                            sd <$> (cast s :: Maybe (Summary Integer))
                  | typeOf s == typeOf (undefined :: Summary Word) =
                            sd <$> (cast s :: Maybe (Summary Word))
                  | typeOf s == typeOf (undefined :: Summary Word8) =
                            sd <$> (cast s :: Maybe (Summary Word8))
                  | typeOf s == typeOf (undefined :: Summary Word16) =
                            sd <$> (cast s :: Maybe (Summary Word16))
                  | typeOf s == typeOf (undefined :: Summary Word32) =
                            sd <$> (cast s :: Maybe (Summary Word32))
                  | typeOf s == typeOf (undefined :: Summary Word64) =
                            sd <$> (cast s :: Maybe (Summary Word64))
                  | otherwise = Nothing
    where
        sd :: ZoomNum a => Summary a -> Summary Double
        sd s' = s' { summaryData = toSummaryDataDouble (summaryData s') }

-- | Coercion of numeric SummaryUTC to type SummaryUTC Double.
toSummaryUTCDouble :: Typeable a => SummaryUTC a -> Maybe (SummaryUTC Double)
toSummaryUTCDouble s | typeOf s == typeOf (undefined :: SummaryUTC Double) =
                                  id (cast s :: Maybe (SummaryUTC Double))
                    | typeOf s == typeOf (undefined :: SummaryUTC Float) =
                                  sd <$> (cast s :: Maybe (SummaryUTC Float))
                    | typeOf s == typeOf (undefined :: SummaryUTC Int) =
                              sd <$> (cast s :: Maybe (SummaryUTC Int))
                    | typeOf s == typeOf (undefined :: SummaryUTC Int8) =
                              sd <$> (cast s :: Maybe (SummaryUTC Int8))
                    | typeOf s == typeOf (undefined :: SummaryUTC Int16) =
                              sd <$> (cast s :: Maybe (SummaryUTC Int16))
                    | typeOf s == typeOf (undefined :: SummaryUTC Int32) =
                              sd <$> (cast s :: Maybe (SummaryUTC Int32))
                    | typeOf s == typeOf (undefined :: SummaryUTC Int64) =
                              sd <$> (cast s :: Maybe (SummaryUTC Int64))
                    | typeOf s == typeOf (undefined :: SummaryUTC Integer) =
                              sd <$> (cast s :: Maybe (SummaryUTC Integer))
                    | typeOf s == typeOf (undefined :: SummaryUTC Word) =
                              sd <$> (cast s :: Maybe (SummaryUTC Word))
                    | typeOf s == typeOf (undefined :: SummaryUTC Word8) =
                              sd <$> (cast s :: Maybe (SummaryUTC Word8))
                    | typeOf s == typeOf (undefined :: SummaryUTC Word16) =
                              sd <$> (cast s :: Maybe (SummaryUTC Word16))
                    | typeOf s == typeOf (undefined :: SummaryUTC Word32) =
                              sd <$> (cast s :: Maybe (SummaryUTC Word32))
                    | typeOf s == typeOf (undefined :: SummaryUTC Word64) =
                              sd <$> (cast s :: Maybe (SummaryUTC Word64))
                    | otherwise = Nothing
    where
        sd :: ZoomNum a => SummaryUTC a -> SummaryUTC Double
        sd s' = s' { summaryUTCData = toSummaryDataDouble (summaryUTCData s') }

toSummaryDataDouble :: ZoomNum a => SummaryData a -> SummaryData Double
toSummaryDataDouble s = numMkSummary
    (realToFrac . numEntry $ s)
    (realToFrac . numExit $ s)
    (realToFrac . numMin $ s)
    (realToFrac . numMax $ s)
    (numAvg s)
    (numRMS s)

----------------------------------------------------------------------

-- | Read the summary of an entire track.
wholeTrackSummaryDouble :: (Functor m, MonadIO m)
                        => TrackNo
                        -> I.Iteratee [Offset Block] m (Summary Double)
wholeTrackSummaryDouble trackNo =
    I.joinI $ filterTracks [trackNo] .  I.joinI . e $ I.last
    where
        e = I.joinI . enumSummaries . I.mapChunks (catMaybes . map toSD)
        toSD :: ZoomSummary -> Maybe (Summary Double)
        toSD (ZoomSummary s) = toSummaryDouble s

-- | Read the summary of an entire track.
wholeTrackSummaryUTCDouble :: (Functor m, MonadIO m)
                           => TrackNo
                           -> I.Iteratee [Offset Block] m (SummaryUTC Double)
wholeTrackSummaryUTCDouble trackNo =
    I.joinI $ filterTracks [trackNo] .  I.joinI . e $ I.last
    where
        e = I.joinI . enumSummariesUTC . I.mapChunks (catMaybes . map toSD)
        toSD :: ZoomSummaryUTC -> Maybe (SummaryUTC Double)
        toSD (ZoomSummaryUTC s) = toSummaryUTCDouble s

enumDouble :: (Functor m, Monad m)
           => I.Enumeratee [Offset Block] [(TimeStamp, Double)] m a
enumDouble = I.joinI . enumPackets . I.mapChunks (concatMap f)
    where
        f :: Packet -> [(TimeStamp, Double)]
        f Packet{..} = zip packetTimeStamps (rawToDouble packetData)

enumUTCDouble :: (Functor m, Monad m)
              => I.Enumeratee [Offset Block] [(UTCTime, Double)] m a
enumUTCDouble = I.joinI . enumPacketsUTC . I.mapChunks (concatMap f)
    where
        f :: PacketUTC -> [(UTCTime, Double)]
        f PacketUTC{..} = zip packetUTCTimeStamps (rawToDouble packetUTCData)

enumSummaryDouble :: (Functor m, Monad m)
                  => Int
                  -> I.Enumeratee [Offset Block] [Summary Double] m a
enumSummaryDouble level =
    I.joinI . enumSummaryLevel level .
    I.mapChunks (catMaybes . map toSD)
    where
        toSD :: ZoomSummary -> Maybe (Summary Double)
        toSD (ZoomSummary s) = toSummaryDouble s

enumSummaryUTCDouble :: (Functor m, Monad m)
                     => Int
                     -> I.Enumeratee [Offset Block] [SummaryUTC Double] m a
enumSummaryUTCDouble level =
    I.joinI . enumSummaryUTCLevel level .
    I.mapChunks (catMaybes . map toSD)
    where
        toSD :: ZoomSummaryUTC -> Maybe (SummaryUTC Double)
        toSD (ZoomSummaryUTC s) = toSummaryUTCDouble s

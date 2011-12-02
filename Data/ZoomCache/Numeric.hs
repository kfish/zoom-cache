{-# LANGUAGE CPP #-}
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

  , toSummarySODouble

  , wholeTrackSummarySODouble
  , enumDouble
  , enumSummarySODouble

  , module Data.ZoomCache
) where

import Control.Applicative ((<$>))
import Control.Monad.Trans (MonadIO)
import Data.ByteString (ByteString)
import Data.Int
import qualified Data.Iteratee as I
import Data.Maybe
import Data.Typeable
import Data.Word
import Data.ZoomCache
import Data.ZoomCache.Numeric.Types
import Data.ZoomCache.Types

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

-- | Coercion of numeric summaries to type SummarySO Double.
toSummarySODouble :: Typeable a => SummarySO a -> Maybe (SummarySO Double)
toSummarySODouble s | typeOf s == typeOf (undefined :: SummarySO Double) =
                                id (cast s :: Maybe (SummarySO Double))
                  | typeOf s == typeOf (undefined :: SummarySO Float) =
                            sd <$> (cast s :: Maybe (SummarySO Float))
                  | typeOf s == typeOf (undefined :: SummarySO Int) =
                            sd <$> (cast s :: Maybe (SummarySO Int))
                  | typeOf s == typeOf (undefined :: SummarySO Int8) =
                            sd <$> (cast s :: Maybe (SummarySO Int8))
                  | typeOf s == typeOf (undefined :: SummarySO Int16) =
                            sd <$> (cast s :: Maybe (SummarySO Int16))
                  | typeOf s == typeOf (undefined :: SummarySO Int32) =
                            sd <$> (cast s :: Maybe (SummarySO Int32))
                  | typeOf s == typeOf (undefined :: SummarySO Int64) =
                            sd <$> (cast s :: Maybe (SummarySO Int64))
                  | typeOf s == typeOf (undefined :: SummarySO Integer) =
                            sd <$> (cast s :: Maybe (SummarySO Integer))
                  | typeOf s == typeOf (undefined :: SummarySO Word) =
                            sd <$> (cast s :: Maybe (SummarySO Word))
                  | typeOf s == typeOf (undefined :: SummarySO Word8) =
                            sd <$> (cast s :: Maybe (SummarySO Word8))
                  | typeOf s == typeOf (undefined :: SummarySO Word16) =
                            sd <$> (cast s :: Maybe (SummarySO Word16))
                  | typeOf s == typeOf (undefined :: SummarySO Word32) =
                            sd <$> (cast s :: Maybe (SummarySO Word32))
                  | typeOf s == typeOf (undefined :: SummarySO Word64) =
                            sd <$> (cast s :: Maybe (SummarySO Word64))
                  | otherwise = Nothing

    where
        sd :: ZoomNum a => SummarySO a -> SummarySO Double
        sd s' = s' { summarySOData = toSummaryDataDouble (summarySOData s') }
        
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
wholeTrackSummarySODouble :: (Functor m, MonadIO m)
                          => [IdentifyCodec]
                          -> TrackNo
                          -> I.Iteratee ByteString m (SummarySO Double)
wholeTrackSummarySODouble identifiers trackNo = I.joinI $ enumCacheFile identifiers .
    I.joinI . filterTracks [trackNo] .  I.joinI . e $ I.last
    where
        e = I.joinI . enumSummarySOs . I.mapChunks (catMaybes . map toSD)
        toSD :: ZoomSummarySO -> Maybe (SummarySO Double)
        toSD (ZoomSummarySO s) = toSummarySODouble s

enumDouble :: (Functor m, MonadIO m)
           => I.Enumeratee [Stream] [(SampleOffset, Double)] m a
enumDouble = I.joinI . enumPacketSOs . I.mapChunks (concatMap f)
    where
        f :: PacketSO -> [(SampleOffset, Double)]
        f PacketSO{..} = zip packetSOSampleOffsets (rawToDouble packetSOData)

enumSummarySODouble :: (Functor m, MonadIO m)
                    => Int
                    -> I.Enumeratee [Stream] [SummarySO Double] m a
enumSummarySODouble level =
    I.joinI . enumSummarySOLevel level .
    I.mapChunks (catMaybes . map toSD)
    where
        toSD :: ZoomSummarySO -> Maybe (SummarySO Double)
        toSD (ZoomSummarySO s) = toSummarySODouble s

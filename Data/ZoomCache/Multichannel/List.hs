{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
{- |
   Module      : Data.ZoomCache.Multichannel.List
   Copyright   : Conrad Parker
   License     : BSD3-style (see LICENSE)

   Maintainer  : Conrad Parker <conrad@metadecks.org>
   Stability   : unstable
   Portability : unknown

Default codec implementation for multichannel values of type [a].

This module implements the interfaces documented in "Data.ZoomCache.Codec".

Multichannel SummaryData is simply a concatenation of n blocks of SummaryData
for type a.

-}
----------------------------------------------------------------------

module Data.ZoomCache.Multichannel.List (
      SummaryData()
    , SummaryWork()

    , wholeTrackSummaryListDouble
    , enumListDouble
    , enumSummaryListDouble

    , wholeTrackSummaryUTCListDouble
    , enumUTCListDouble
    , enumSummaryUTCListDouble
)where

import Control.Applicative ((<$>))
import Control.Monad.Trans (MonadIO)
import Data.Int
import qualified Data.Iteratee as I
import Data.Maybe
import Data.Time (UTCTime)
import Data.Typeable
import Data.TypeLevel.Num hiding ((==))
import Data.Word

import Data.Iteratee.ZoomCache
import Data.Offset
import Data.ZoomCache.Codec
import Data.ZoomCache.Multichannel.NList
import Data.ZoomCache.NList
import Data.ZoomCache.Numeric

----------------------------------------------------------------------

instance (ZoomWrite a, ZoomWritable a) => ZoomWrite [a] where
    write = writeList

writeList :: (ZoomWrite a, ZoomWritable a) => TrackNo -> [a] -> ZoomW ()
writeList tn xs = reifyIntegral (length xs) (\n -> write tn (NList n xs))

instance (ZoomWrite a, ZoomWritable a) => ZoomWrite (SampleOffset, [a]) where
    write = writeSOList

writeSOList :: (ZoomWrite a, ZoomWritable a) => TrackNo -> (SampleOffset, [a]) -> ZoomW ()
writeSOList tn (ts, xs) = reifyIntegral (length xs) (\n -> write tn (ts, (NList n xs)))

----------------------------------------------------------------------

rawToListDouble :: ZoomRaw -> [[Double]]
rawToListDouble (ZoomRaw xs) | not (null d) = [d]
                             | typeOf xs == typeOf (undefined :: [NList D1 Double]) =
                                             l (cast xs :: Maybe [NList D1 Double])
                             | typeOf xs == typeOf (undefined :: [NList D1 Float]) =
                                             f (cast xs :: Maybe [NList D1 Float])
                             | typeOf xs == typeOf (undefined :: [NList D1 Int]) =
                                             f (cast xs :: Maybe [NList D1 Int])
                             | typeOf xs == typeOf (undefined :: [NList D1 Int8]) =
                                             f (cast xs :: Maybe [NList D1 Int8])
                             | typeOf xs == typeOf (undefined :: [NList D1 Int16]) =
                                             f (cast xs :: Maybe [NList D1 Int16])
                             | typeOf xs == typeOf (undefined :: [NList D1 Int32]) =
                                             f (cast xs :: Maybe [NList D1 Int32])
                             | typeOf xs == typeOf (undefined :: [NList D1 Int64]) =
                                             f (cast xs :: Maybe [NList D1 Int64])
                             | typeOf xs == typeOf (undefined :: [NList D1 Integer]) =
                                             f (cast xs :: Maybe [NList D1 Integer])
                             | typeOf xs == typeOf (undefined :: [NList D1 Word]) =
                                             f (cast xs :: Maybe [NList D1 Word])
                             | typeOf xs == typeOf (undefined :: [NList D1 Word8]) =
                                             f (cast xs :: Maybe [NList D1 Word8])
                             | typeOf xs == typeOf (undefined :: [NList D1 Word16]) =
                                             f (cast xs :: Maybe [NList D1 Word16])
                             | typeOf xs == typeOf (undefined :: [NList D1 Word32]) =
                                             f (cast xs :: Maybe [NList D1 Word32])
                             | typeOf xs == typeOf (undefined :: [NList D1 Word64]) =
                                             f (cast xs :: Maybe [NList D1 Word64])
                             | otherwise = []
    where
        d = rawToDouble (ZoomRaw xs)
        l :: Maybe [NList D1 a] -> [[a]]
        l = maybe [] (map nListToList)
        f :: (ZoomReadable a, Real a) => Maybe [NList D1 a] -> [[Double]]
        f = map (rawToDouble . ZoomRaw) . l

toSummaryListDouble :: Typeable a => Summary a -> Maybe [Summary Double]
toSummaryListDouble s | isJust sd = (:[]) <$> sd
                      | typeOf s == typeOf (undefined :: Summary (NList D1 Double)) =
                                sl <$> (cast s :: Maybe (Summary (NList D1 Double)))
                      | typeOf s == typeOf (undefined :: Summary (NList D1 Float)) =
                               sld <$> (cast s :: Maybe (Summary (NList D1 Float)))
                      | typeOf s == typeOf (undefined :: Summary (NList D1 Int)) =
                               sld <$> (cast s :: Maybe (Summary (NList D1 Int)))
                      | typeOf s == typeOf (undefined :: Summary (NList D1 Int8)) =
                               sld <$> (cast s :: Maybe (Summary (NList D1 Int8)))
                      | typeOf s == typeOf (undefined :: Summary (NList D1 Int16)) =
                               sld <$> (cast s :: Maybe (Summary (NList D1 Int16)))
                      | typeOf s == typeOf (undefined :: Summary (NList D1 Int32)) =
                               sld <$> (cast s :: Maybe (Summary (NList D1 Int32)))
                      | typeOf s == typeOf (undefined :: Summary (NList D1 Int64)) =
                               sld <$> (cast s :: Maybe (Summary (NList D1 Int64)))
                      | typeOf s == typeOf (undefined :: Summary (NList D1 Integer)) =
                               sld <$> (cast s :: Maybe (Summary (NList D1 Integer)))
                      | typeOf s == typeOf (undefined :: Summary (NList D1 Word)) =
                               sld <$> (cast s :: Maybe (Summary (NList D1 Word)))
                      | typeOf s == typeOf (undefined :: Summary (NList D1 Word8)) =
                               sld <$> (cast s :: Maybe (Summary (NList D1 Word8)))
                      | typeOf s == typeOf (undefined :: Summary (NList D1 Word16)) =
                               sld <$> (cast s :: Maybe (Summary (NList D1 Word16)))
                      | typeOf s == typeOf (undefined :: Summary (NList D1 Word32)) =
                               sld <$> (cast s :: Maybe (Summary (NList D1 Word32)))
                      | typeOf s == typeOf (undefined :: Summary (NList D1 Word64)) =
                               sld <$> (cast s :: Maybe (Summary (NList D1 Word64)))
                      | otherwise = Nothing
    where
        sd = toSummaryDouble s
        sl :: Summary (NList D1 a) -> [Summary a]
        sl = summaryNListToList
        sld :: Typeable a => Summary (NList D1 a) -> [Summary Double]
        sld = catMaybes . map toSummaryDouble . sl

toSummaryUTCListDouble :: Typeable a => SummaryUTC a -> Maybe [SummaryUTC Double]
toSummaryUTCListDouble s | isJust sd = (:[]) <$> sd
                         | typeOf s == typeOf (undefined :: SummaryUTC (NList D1 Double)) =
                                   sl <$> (cast s :: Maybe (SummaryUTC (NList D1 Double)))
                         | typeOf s == typeOf (undefined :: SummaryUTC (NList D1 Float)) =
                                  sld <$> (cast s :: Maybe (SummaryUTC (NList D1 Float)))
                         | typeOf s == typeOf (undefined :: SummaryUTC (NList D1 Int)) =
                                  sld <$> (cast s :: Maybe (SummaryUTC (NList D1 Int)))
                         | typeOf s == typeOf (undefined :: SummaryUTC (NList D1 Int8)) =
                                  sld <$> (cast s :: Maybe (SummaryUTC (NList D1 Int8)))
                         | typeOf s == typeOf (undefined :: SummaryUTC (NList D1 Int16)) =
                                  sld <$> (cast s :: Maybe (SummaryUTC (NList D1 Int16)))
                         | typeOf s == typeOf (undefined :: SummaryUTC (NList D1 Int32)) =
                                  sld <$> (cast s :: Maybe (SummaryUTC (NList D1 Int32)))
                         | typeOf s == typeOf (undefined :: SummaryUTC (NList D1 Int64)) =
                                  sld <$> (cast s :: Maybe (SummaryUTC (NList D1 Int64)))
                         | typeOf s == typeOf (undefined :: SummaryUTC (NList D1 Integer)) =
                                  sld <$> (cast s :: Maybe (SummaryUTC (NList D1 Integer)))
                         | typeOf s == typeOf (undefined :: SummaryUTC (NList D1 Word)) =
                                  sld <$> (cast s :: Maybe (SummaryUTC (NList D1 Word)))
                         | typeOf s == typeOf (undefined :: SummaryUTC (NList D1 Word8)) =
                                  sld <$> (cast s :: Maybe (SummaryUTC (NList D1 Word8)))
                         | typeOf s == typeOf (undefined :: SummaryUTC (NList D1 Word16)) =
                                  sld <$> (cast s :: Maybe (SummaryUTC (NList D1 Word16)))
                         | typeOf s == typeOf (undefined :: SummaryUTC (NList D1 Word32)) =
                                  sld <$> (cast s :: Maybe (SummaryUTC (NList D1 Word32)))
                         | typeOf s == typeOf (undefined :: SummaryUTC (NList D1 Word64)) =
                                  sld <$> (cast s :: Maybe (SummaryUTC (NList D1 Word64)))
                         | otherwise = Nothing
    where
        sd = toSummaryUTCDouble s
        sl :: SummaryUTC (NList D1 a) -> [SummaryUTC a]
        sl = summaryUTCNListToList
        sld :: Typeable a => SummaryUTC (NList D1 a) -> [SummaryUTC Double]
        sld = catMaybes . map toSummaryUTCDouble . sl

----------------------------------------------------------------------

-- | Read the summary of an entire track.
wholeTrackSummaryListDouble :: (Functor m, MonadIO m)
                            => TrackNo
                            -> I.Iteratee [Offset Block] m [Summary Double]
wholeTrackSummaryListDouble trackNo =
    I.joinI $ filterTracks [trackNo] .  I.joinI . e $ I.last
    where
        e = I.joinI . enumSummaries . I.mapChunks (catMaybes . map toSLD)
        toSLD :: ZoomSummary -> Maybe [Summary Double]
        toSLD (ZoomSummary s) = toSummaryListDouble s

enumListDouble :: (Functor m, Monad m)
               => I.Enumeratee [Offset Block] [(TimeStamp, [Double])] m a
enumListDouble = I.joinI . enumPackets . I.mapChunks (concatMap f)
    where
        f :: Packet -> [(TimeStamp, [Double])]
        f Packet{..} = zip packetTimeStamps (rawToListDouble packetData)

enumSummaryListDouble :: (Functor m, Monad m)
                      => Int
                      -> I.Enumeratee [Offset Block] [[Summary Double]] m a
enumSummaryListDouble level =
    I.joinI . enumSummaryLevel level .
    I.mapChunks (catMaybes . map toSLD)
    where
        toSLD :: ZoomSummary -> Maybe [Summary Double]
        toSLD (ZoomSummary s) = toSummaryListDouble s

----------------------------------------------------------------------

-- | Read the summary of an entire track.
wholeTrackSummaryUTCListDouble :: (Functor m, MonadIO m)
                               => TrackNo
                               -> I.Iteratee [Offset Block] m [SummaryUTC Double]
wholeTrackSummaryUTCListDouble trackNo =
    I.joinI $ filterTracks [trackNo] .  I.joinI . e $ I.last
    where
        e = I.joinI . enumSummariesUTC . I.mapChunks (catMaybes . map toSLD)
        toSLD :: ZoomSummaryUTC -> Maybe [SummaryUTC Double]
        toSLD (ZoomSummaryUTC s) = toSummaryUTCListDouble s

enumUTCListDouble :: (Functor m, Monad m)
                  => I.Enumeratee [Offset Block] [(UTCTime, [Double])] m a
enumUTCListDouble = I.joinI . enumPacketsUTC . I.mapChunks (concatMap f)
    where
        f :: PacketUTC -> [(UTCTime, [Double])]
        f PacketUTC{..} = zip packetUTCTimeStamps (rawToListDouble packetUTCData)

enumSummaryUTCListDouble :: (Functor m, Monad m)
                         => Int
                         -> I.Enumeratee [Offset Block] [[SummaryUTC Double]] m a
enumSummaryUTCListDouble level =
    I.joinI . enumSummaryUTCLevel level .
    I.mapChunks (catMaybes . map toSLD)
    where
        toSLD :: ZoomSummaryUTC -> Maybe [SummaryUTC Double]
        toSLD (ZoomSummaryUTC s) = toSummaryUTCListDouble s

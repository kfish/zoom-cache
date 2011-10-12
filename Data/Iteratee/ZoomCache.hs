{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      : Data.ZoomCache.Write
-- Copyright   : Conrad Parker
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Conrad Parker <conrad@metadecks.org>
-- Stability   : unstable
-- Portability : unknown
--
-- Iteratee reading of ZoomCache files.
----------------------------------------------------------------------

module Data.Iteratee.ZoomCache (
    -- * Types
      Stream(..)

    -- Parsing iteratees
    , iterHeaders

    -- * Enumeratee
    , enumStream

    -- * Iteratee maps
    , mapStream
    , mapPackets
    , mapSummaries
) where

import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import Control.Monad.Trans (MonadIO)
import qualified Data.ByteString.Lazy as L
import Data.Int
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Iteratee (Iteratee)
import qualified Data.Iteratee as I
import qualified Data.Iteratee.ListLike as LL
import Data.Maybe
import Data.Ratio
import Data.Word
import Unsafe.Coerce (unsafeCoerce)

import Data.ZoomCache.Common
import Data.ZoomCache.Packet
import Data.ZoomCache.Summary

----------------------------------------------------------------------

data Stream =
    StreamPacket
        { _strmTrack   :: TrackNo
        , _strmPacket  :: Packet
        }
    | StreamSummary
        { _strmTrack   :: TrackNo
        , _strmSummary :: Summary
        }
    | StreamNull

instance LL.Nullable Stream where
    nullC StreamNull = True
    nullC _          = False

instance LL.NullPoint Stream where
    empty = StreamNull

----------------------------------------------------------------------

mapStream :: (Functor m, MonadIO m)
          => (Stream -> m ())
          -> Iteratee [Word8] m ()
mapStream f = do
    fi <- iterHeaders
    I.joinI . enumStream fi . I.mapChunksM_ $ f
    return ()

mapPackets :: (Functor m, MonadIO m)
           => (Packet -> m ())
           -> Iteratee [Word8] m ()
mapPackets f = mapStream process
    where
        process (StreamPacket _ p) = f p
        process _                  = return ()

mapSummaries :: (Functor m, MonadIO m)
             => (Summary -> m ())
             -> Iteratee [Word8] m ()
mapSummaries f = mapStream process
    where
        process (StreamSummary _ s) = f s
        process _                   = return ()

enumStream :: (Functor m, MonadIO m)
            => FileInfo
            -> I.Enumeratee [Word8] Stream m a
enumStream = I.unfoldConvStream go
    where
        go :: (Functor m, MonadIO m)
           => FileInfo
           -> Iteratee [Word8] m (FileInfo, Stream)
        go fi = do
            header <- I.joinI $ I.takeUpTo 8 I.stream2list
            case parseHeader (L.pack header) of
                Just PacketHeader -> do
                    (trackNo, packet) <- readPacket (fiSpecs fi)
                    return (fi, StreamPacket trackNo (fromJust packet))
                Just SummaryHeader -> do
                    (trackNo, summary) <- readSummary (fiSpecs fi)
                    return (fi, StreamSummary trackNo (fromJust summary))
                _ -> return (fi, StreamNull)

------------------------------------------------------------

parseHeader :: L.ByteString -> Maybe HeaderType
parseHeader h
    | h == globalHeader  = Just GlobalHeader
    | h == trackHeader   = Just TrackHeader
    | h == packetHeader  = Just PacketHeader
    | h == summaryHeader = Just SummaryHeader
    | otherwise          = Nothing

------------------------------------------------------------
-- Global, track headers

iterHeaders :: (Functor m, MonadIO m)
            => I.Iteratee [Word8] m FileInfo
iterHeaders = iterGlobal >>= go
    where
        iterGlobal :: (Functor m, MonadIO m)
                   => Iteratee [Word8] m FileInfo
        iterGlobal = do
            header <- I.joinI $ I.takeUpTo 8 I.stream2list
            case parseHeader (L.pack header) of
                Just GlobalHeader -> mkFileInfo <$> readGlobalHeader
                _                 -> error "No global header"

        go :: (Functor m, MonadIO m)
           => FileInfo
           -> Iteratee [Word8] m FileInfo
        go fi = do
            header <- I.joinI $ I.takeUpTo 8 I.stream2list
            case parseHeader (L.pack header) of
                Just TrackHeader -> do
                    (trackNo, spec) <- readTrackHeader
                    let fi' = fi{fiSpecs = IM.insert trackNo spec (fiSpecs fi)}
                    if (fiFull fi')
                        then return fi'
                        else go fi'
                _ -> return fi

readGlobalHeader :: (Functor m, MonadIO m) => Iteratee [Word8] m Global
readGlobalHeader = do
    v <- readVersion
    n <- zReadInt32
    p <- readRational64
    b <- readRational64
    _u <- L.pack <$> (I.joinI $ I.takeUpTo 20 I.stream2list)
    return $ Global v n p b Nothing

readTrackHeader :: (Functor m, MonadIO m) => Iteratee [Word8] m (TrackNo, TrackSpec)
readTrackHeader = do
    trackNo <- zReadInt32
    trackType <- readTrackType
    drType <- readDataRateType

    rate <- readRational64

    byteLength <- zReadInt32
    name <- L.pack <$> (I.joinI $ I.takeUpTo byteLength I.stream2list)

    let spec = TrackSpec trackType drType rate name

    return (trackNo, spec)

------------------------------------------------------------
-- Packet, Summary reading

readPacket :: (Functor m, MonadIO m)
           => IntMap TrackSpec
           -> Iteratee [Word8] m (TrackNo, Maybe Packet)
readPacket specs = do
    trackNo <- zReadInt32
    entryTime <- TS <$> zReadInt32
    exitTime <- TS <$> zReadInt32
    byteLength <- zReadInt32
    count <- zReadInt32
    packet <- case IM.lookup trackNo specs of
        Just TrackSpec{..} -> do
            d <- case specType of
                ZDouble -> do
                    PDDouble <$> replicateM count zReadFloat64be
                ZInt -> do
                    PDInt <$> replicateM count zReadInt32
            ts <- map TS <$> case specDRType of
                ConstantDR -> do
                    return $ take count [unTS entryTime ..]
                VariableDR -> do
                    replicateM count zReadInt32
            return $ Just (Packet trackNo entryTime exitTime count d ts)
        Nothing -> do
            I.drop byteLength
            return Nothing
    return (trackNo, packet)

readSummary :: (Functor m, MonadIO m)
            => IntMap TrackSpec
            -> Iteratee [Word8] m (TrackNo, Maybe Summary)
readSummary specs = do
    trackNo <- zReadInt32
    lvl <- zReadInt32
    entryTime <- TS <$> zReadInt32
    exitTime <- TS <$> zReadInt32
    byteLength <- zReadInt32

    summary <- case IM.lookup trackNo specs of
        Just TrackSpec{..} -> do
            case specType of
                ZDouble -> do
                    let n = flip div 8 byteLength
                    [en,ex,mn,mx,avg,rms] <- replicateM n zReadFloat64be
                    return $ Just (SummaryDouble trackNo lvl entryTime exitTime
                                       en ex mn mx avg rms)
                ZInt -> do
                    [en,ex,mn,mx] <- replicateM 4 zReadInt32
                    [avg,rms] <- replicateM 2 zReadFloat64be
                    return $ Just (SummaryInt trackNo lvl entryTime exitTime
                                       en ex mn mx avg rms)
        Nothing -> do
            I.drop byteLength
            return Nothing
    return (trackNo, summary)

----------------------------------------------------------------------
-- zoom-cache datatype parsers

readVersion :: (Functor m, MonadIO m) => Iteratee [Word8] m Version
readVersion = do
    vMaj <- zReadInt16
    vMin <- zReadInt16
    return $ Version vMaj vMin

readTrackType :: (Functor m, MonadIO m) => Iteratee [Word8] m TrackType
readTrackType = do
    n <- zReadInt16
    case n of
        0 -> return ZDouble
        1 -> return ZInt
        _ -> error "Bad tracktype"

readDataRateType :: (Functor m, MonadIO m) => Iteratee [Word8] m DataRateType
readDataRateType = do
    n <- zReadInt16
    case n of
        0 -> return ConstantDR
        1 -> return VariableDR
        _ -> error "Bad data rate type"

----------------------------------------------------------------------

zReadInt16 :: (Functor m, MonadIO m) => Iteratee [Word8] m Int
zReadInt16 = fromIntegral . u16_to_s16 <$> I.endianRead2 I.MSB
    where
        u16_to_s16 :: Word16 -> Int16
        u16_to_s16 = fromIntegral

zReadInt32 :: (Functor m, MonadIO m) => Iteratee [Word8] m Int
zReadInt32 = fromIntegral . u32_to_s32 <$> I.endianRead4 I.MSB
    where
        u32_to_s32 :: Word32 -> Int32
        u32_to_s32 = fromIntegral

zReadInt64 :: (Functor m, MonadIO m) => Iteratee [Word8] m Int
zReadInt64 = fromIntegral . u64_to_s64 <$> I.endianRead8 I.MSB
    where
        u64_to_s64 :: Word64 -> Int64
        u64_to_s64 = fromIntegral

zReadFloat64be :: (Functor m, MonadIO m) => Iteratee [Word8] m Double
zReadFloat64be = do
    n <- I.endianRead8 I.MSB
    return (unsafeCoerce n :: Double)

readRational64 :: (Functor m, MonadIO m) => Iteratee [Word8] m Rational
readRational64 = do
    num <- zReadInt64
    den <- zReadInt64
    if (den == 0)
        then return 0
        else return $ (fromIntegral num) % (fromIntegral den)

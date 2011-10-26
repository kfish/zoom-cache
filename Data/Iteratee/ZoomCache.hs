{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

    -- * Parsing iteratees
    , iterHeaders

    -- * Enumeratee
    , enumCacheFile
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
import Data.Word

import Data.Iteratee.ZoomCache.Utils
import Data.ZoomCache.Common
import Data.ZoomCache.Internal
import Data.ZoomCache.Types

-- XXX: Remove these
import Data.ZoomCache.Double()
import Data.ZoomCache.Int()

----------------------------------------------------------------------

data Stream =
    StreamPacket
        { strmFile    :: CacheFile
        , strmTrack   :: TrackNo
        , strmPacket  :: Packet
        }
    | StreamSummary
        { strmFile    :: CacheFile
        , strmTrack   :: TrackNo
        , strmSummary :: ZoomSummary
        }
    | StreamNull

instance LL.Nullable Stream where
    nullC StreamNull = True
    nullC _          = False

instance LL.NullPoint Stream where
    empty = StreamNull

----------------------------------------------------------------------

-- | An enumeratee of a zoom-cache file, from the global header onwards.
-- The global and track headers will be transparently read, and the 
-- 'CacheFile' visible in the 'Stream' elements.
enumCacheFile :: (Functor m, MonadIO m)
              => I.Enumeratee [Word8] Stream m a
enumCacheFile iter = do
    fi <- iterHeaders
    enumStream fi iter

-- | An enumeratee of zoom-cache data, after global and track headers
-- have been read, or if the 'CacheFile' has been acquired elsewhere.
enumStream :: (Functor m, MonadIO m)
            => CacheFile
            -> I.Enumeratee [Word8] Stream m a
enumStream = I.unfoldConvStream go
    where
        go :: (Functor m, MonadIO m)
           => CacheFile
           -> Iteratee [Word8] m (CacheFile, Stream)
        go cf = do
            header <- I.joinI $ I.takeUpTo 8 I.stream2list
            case parseHeader (L.pack header) of
                Just PacketHeader -> do
                    (trackNo, packet) <- readPacket (cfSpecs cf)
                    return (cf, StreamPacket cf trackNo (fromJust packet))
                Just SummaryHeader -> do
                    (trackNo, summary) <- readSummaryBlock (cfSpecs cf)
                    return (cf, StreamSummary cf trackNo (fromJust summary))
                _ -> return (cf, StreamNull)

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

-- | Parse only the global and track headers of a zoom-cache file, returning
-- a 'CacheFile'
iterHeaders :: (Functor m, MonadIO m)
            => I.Iteratee [Word8] m CacheFile
iterHeaders = iterGlobal >>= go
    where
        iterGlobal :: (Functor m, MonadIO m)
                   => Iteratee [Word8] m CacheFile
        iterGlobal = do
            header <- I.joinI $ I.takeUpTo 8 I.stream2list
            case parseHeader (L.pack header) of
                Just GlobalHeader -> mkCacheFile <$> readGlobalHeader
                _                 -> error "No global header"

        go :: (Functor m, MonadIO m)
           => CacheFile
           -> Iteratee [Word8] m CacheFile
        go fi = do
            header <- I.joinI $ I.takeUpTo 8 I.stream2list
            case parseHeader (L.pack header) of
                Just TrackHeader -> do
                    (trackNo, spec) <- readTrackHeader
                    let fi' = fi{cfSpecs = IM.insert trackNo spec (cfSpecs fi)}
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
    entryTime <- TS <$> zReadInt64
    exitTime <- TS <$> zReadInt64
    byteLength <- zReadInt32
    count <- zReadInt32
    packet <- case IM.lookup trackNo specs of
        Just TrackSpec{..} -> do
            let readTS = readTimeStamps specDRType count entryTime
            case specType of
                ZDouble -> do
                    (d :: [Double]) <- replicateM count readRaw
                    ts <- readTS
                    return . Just $
                        (Packet trackNo entryTime exitTime count
                            (ZoomRaw . fromList $ d) ts)
                ZInt -> do
                    (d :: [Int]) <- replicateM count readRaw
                    ts <- readTS
                    return . Just $
                        (Packet trackNo entryTime exitTime count
                            (ZoomRaw . fromList $ d) ts)
        Nothing -> do
            I.drop byteLength
            return Nothing
    return (trackNo, packet)
    where
        readTimeStamps :: (Functor m, MonadIO m)
                       => DataRateType -> Int -> TimeStamp
                       -> Iteratee [Word8] m [TimeStamp]
        readTimeStamps drType count entry = map TS <$> case drType of
            ConstantDR -> do
                return $ take count [unTS entry ..]
            VariableDR -> do
                replicateM count zReadInt64

readSummaryBlock :: (Functor m, MonadIO m)
                 => IntMap TrackSpec
                 -> Iteratee [Word8] m (TrackNo, Maybe ZoomSummary)
readSummaryBlock specs = do
    trackNo <- zReadInt32
    lvl <- zReadInt32
    entryTime <- TS <$> zReadInt64
    exitTime <- TS <$> zReadInt64
    byteLength <- zReadInt32

    summary <- case IM.lookup trackNo specs of
        Just TrackSpec{..} -> do
            case specType of
                ZDouble -> do
                    (sd :: SummaryData Double) <- readSummary
                    return . Just . ZoomSummary $
                        Summary trackNo lvl entryTime exitTime sd
                ZInt -> do
                    (sd :: SummaryData Int) <- readSummary
                    return . Just . ZoomSummary $
                        Summary trackNo lvl entryTime exitTime sd
        Nothing -> do
            I.drop byteLength
            return Nothing
    return (trackNo, summary)

----------------------------------------------------------------------
-- Convenience functions

-- | Map a monadic 'Stream' processing function over an entire zoom-cache file.
mapStream :: (Functor m, MonadIO m)
          => (Stream -> m ())
          -> Iteratee [Word8] m ()
mapStream = I.joinI . enumCacheFile . I.mapChunksM_

-- | Map a monadic 'Packet' processing function over an entire zoom-cache file.
mapPackets :: (Functor m, MonadIO m)
           => (Packet -> m ())
           -> Iteratee [Word8] m ()
mapPackets f = mapStream process
    where
        process StreamPacket{..} = f strmPacket
        process _                = return ()

-- | Map a monadic 'Summary' processing function over an entire zoom-cache file.
mapSummaries :: (Functor m, MonadIO m)
             => (ZoomSummary -> m ())
             -> Iteratee [Word8] m ()
mapSummaries f = mapStream process
    where
        process StreamSummary{..} = f strmSummary
        process _                 = return ()

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

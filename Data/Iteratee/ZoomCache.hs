{-# LANGUAGE FlexibleContexts #-}
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

import Control.Applicative
import Control.Monad (msum, replicateM)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Int
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Iteratee (Iteratee)
import qualified Data.Iteratee as I
import qualified Data.ListLike as LL
import Data.Maybe
import Data.Word

import Data.Iteratee.ZoomCache.Utils
import Data.ZoomCache.Common
import Data.ZoomCache.Format
import Data.ZoomCache.Types

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

instance I.Nullable Stream where
    nullC StreamNull = True
    nullC _          = False

instance I.NullPoint Stream where
    empty = StreamNull

----------------------------------------------------------------------

-- | An enumeratee of a zoom-cache file, from the global header onwards.
-- The global and track headers will be transparently read, and the 
-- 'CacheFile' visible in the 'Stream' elements.
enumCacheFile :: (I.Nullable s, LL.ListLike s Word8, Functor m, MonadIO m)
              => [IdentifyTrack]
              -> I.Enumeratee s Stream m a
enumCacheFile mappings iter = do
    fi <- iterHeaders mappings
    enumStream fi iter

-- | An enumeratee of zoom-cache data, after global and track headers
-- have been read, or if the 'CacheFile' has been acquired elsewhere.
enumStream :: (I.Nullable s, LL.ListLike s Word8, Functor m, MonadIO m)
            => CacheFile
            -> I.Enumeratee s Stream m a
enumStream = I.unfoldConvStream go
    where
        go :: (I.Nullable s, LL.ListLike s Word8, Functor m, MonadIO m)
           => CacheFile
           -> Iteratee s m (CacheFile, Stream)
        go cf = do
            header <- I.joinI $ I.takeUpTo 8 I.stream2list
            case parseHeader (B.pack header) of
                Just PacketHeader -> do
                    (trackNo, packet) <- readPacket (cfSpecs cf)
                    return (cf, StreamPacket cf trackNo (fromJust packet))
                Just SummaryHeader -> do
                    (trackNo, summary) <- readSummaryBlock (cfSpecs cf)
                    return (cf, StreamSummary cf trackNo (fromJust summary))
                _ -> return (cf, StreamNull)

------------------------------------------------------------

data HeaderType = GlobalHeader | TrackHeader | PacketHeader | SummaryHeader

parseHeader :: ByteString -> Maybe HeaderType
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
iterHeaders :: (I.Nullable s, LL.ListLike s Word8, Functor m, MonadIO m)
            => [IdentifyTrack]
            -> I.Iteratee s m CacheFile
iterHeaders mappings = iterGlobal >>= go
    where
        iterGlobal :: (I.Nullable s, LL.ListLike s Word8, Functor m, MonadIO m)
                   => Iteratee s m CacheFile
        iterGlobal = do
            header <- I.joinI $ I.takeUpTo 8 I.stream2list
            case parseHeader (B.pack header) of
                Just GlobalHeader -> mkCacheFile <$> readGlobalHeader
                _                 -> error "No global header"

        go :: (I.Nullable s, LL.ListLike s Word8, Functor m, MonadIO m)
           => CacheFile
           -> Iteratee s m CacheFile
        go fi = do
            header <- I.joinI $ I.takeUpTo 8 I.stream2list
            case parseHeader (B.pack header) of
                Just TrackHeader -> do
                    (trackNo, spec) <- readTrackHeader mappings
                    let fi' = fi{cfSpecs = IM.insert trackNo spec (cfSpecs fi)}
                    if (fiFull fi')
                        then return fi'
                        else go fi'
                _ -> return fi

readGlobalHeader :: (I.Nullable s, LL.ListLike s Word8, Functor m, MonadIO m)
                 => Iteratee s m Global
readGlobalHeader = do
    v <- readVersion
    liftIO $ print v
    n <- readInt32be
    p <- readRational64be
    b <- readRational64be
    _u <- B.pack <$> (I.joinI $ I.takeUpTo 20 I.stream2list)
    return $ Global v n p b Nothing

readTrackHeader :: (I.Nullable s, LL.ListLike s Word8, Functor m, MonadIO m)
                => [IdentifyTrack]
                -> Iteratee s m (TrackNo, TrackSpec)
readTrackHeader mappings = do
    trackNo <- readInt32be
    trackType <- readTrackType mappings
    drType <- readDataRateType
    rate <- readRational64be
    byteLength <- readInt32be
    name <- B.pack <$> (I.joinI $ I.takeUpTo byteLength I.stream2list)

    return (trackNo, TrackSpec trackType drType rate name)

------------------------------------------------------------
-- Packet, Summary reading

readPacket :: (I.Nullable s, LL.ListLike s Word8, Functor m, MonadIO m)
           => IntMap TrackSpec
           -> Iteratee s m (TrackNo, Maybe Packet)
readPacket specs = do
    trackNo <- readInt32be
    entryTime <- TS <$> readInt64be
    exitTime <- TS <$> readInt64be
    count <- readInt32be
    byteLength <- readInt32be
    packet <- case IM.lookup trackNo specs of
        Just TrackSpec{..} -> do
            let readTS = readTimeStamps specDRType count entryTime
            d <- readRawTT specType count
            ts <- readTS
            return . Just $
                (Packet trackNo entryTime exitTime count d ts)
        Nothing -> do
            I.drop byteLength
            return Nothing
    return (trackNo, packet)
    where
        readRawTT :: (I.Nullable s, LL.ListLike s Word8,
                      Functor m, MonadIO m)
                  => TrackType -> Int -> Iteratee s m ZoomRaw
        readRawTT (TT a) count = ZoomRaw <$> replicateM count (readRawAs a)

        readRawAs :: (ZoomReadable a, I.Nullable s, LL.ListLike s Word8,
                      Functor m, MonadIO m)
                  => a -> Iteratee s m a
        readRawAs = const readRaw

        readTimeStamps :: (I.Nullable s, LL.ListLike s Word8, Functor m, MonadIO m)
                       => DataRateType -> Int -> TimeStamp
                       -> Iteratee s m [TimeStamp]
        readTimeStamps drType count entry = map TS <$> case drType of
            ConstantDR -> do
                return $ take count [unTS entry ..]
            VariableDR -> do
                replicateM count readInt64be

readSummaryBlock :: (I.Nullable s, LL.ListLike s Word8, Functor m, MonadIO m)
                 => IntMap TrackSpec
                 -> Iteratee s m (TrackNo, Maybe ZoomSummary)
readSummaryBlock specs = do
    trackNo <- readInt32be
    lvl <- readInt32be
    entryTime <- TS <$> readInt64be
    exitTime <- TS <$> readInt64be
    byteLength <- readInt32be

    summary <- case IM.lookup trackNo specs of
        Just TrackSpec{..} -> do
            sd <- readSummaryTT specType trackNo lvl entryTime exitTime
            return $ Just sd
        Nothing -> do
            I.drop byteLength
            return Nothing
    return (trackNo, summary)
    where
        readSummaryTT :: (I.Nullable s, LL.ListLike s Word8,
                          Functor m, MonadIO m)
                      => TrackType -> TrackNo -> Int -> TimeStamp -> TimeStamp
                      -> Iteratee s m ZoomSummary
        readSummaryTT (TT a) trackNo lvl entryTime exitTime = do
            ZoomSummary <$> (Summary trackNo lvl entryTime exitTime <$> readSummaryAs a)

        readSummaryAs :: (ZoomReadable a, I.Nullable s, LL.ListLike s Word8,
                          Functor m, MonadIO m)
                      => a -> Iteratee s m (SummaryData a)
        readSummaryAs = const readSummary


----------------------------------------------------------------------
-- Convenience functions

-- | Map a monadic 'Stream' processing function over an entire zoom-cache file.
mapStream :: (I.Nullable s, LL.ListLike s Word8, Functor m, MonadIO m)
          => [IdentifyTrack]
          -> (Stream -> m ())
          -> Iteratee s m ()
mapStream mappings = I.joinI . enumCacheFile mappings . I.mapChunksM_

-- | Map a monadic 'Packet' processing function over an entire zoom-cache file.
mapPackets :: (I.Nullable s, LL.ListLike s Word8, Functor m, MonadIO m)
           => [IdentifyTrack]
           -> (Packet -> m ())
           -> Iteratee s m ()
mapPackets mappings f = mapStream mappings process
    where
        process StreamPacket{..} = f strmPacket
        process _                = return ()

-- | Map a monadic 'Summary' processing function over an entire zoom-cache file.
mapSummaries :: (I.Nullable s, LL.ListLike s Word8, Functor m, MonadIO m)
             => [IdentifyTrack]
             -> (ZoomSummary -> m ())
             -> Iteratee s m ()
mapSummaries mappings f = mapStream mappings process
    where
        process StreamSummary{..} = f strmSummary
        process _                 = return ()

----------------------------------------------------------------------
-- zoom-cache datatype parsers

readVersion :: (I.Nullable s, LL.ListLike s Word8, Functor m, MonadIO m)
            => Iteratee s m Version
readVersion = Version <$> readInt16be <*> readInt16be

readTrackType :: (I.Nullable s, LL.ListLike s Word8, Functor m, MonadIO m)
              => [IdentifyTrack]
              -> Iteratee s m TrackType
readTrackType mappings = do
    tt <- B.pack <$> (I.joinI $ I.takeUpTo 8 I.stream2list)
    maybe (error "Unknown track type") return (parseTrackType mappings tt)

parseTrackType :: [IdentifyTrack] -> IdentifyTrack
parseTrackType mappings h = msum . map ($ h) $ mappings

readDataRateType :: (I.Nullable s, LL.ListLike s Word8, Functor m, MonadIO m)
                 => Iteratee s m DataRateType
readDataRateType = do
    n <- readInt16be
    case n of
        0 -> return ConstantDR
        1 -> return VariableDR
        _ -> error "Bad data rate type"

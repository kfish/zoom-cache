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

module Data.Iteratee.ZoomCache where

import Control.Applicative
import Control.Monad (msum, replicateM)
import Control.Monad.Trans (MonadIO)
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Functor.Identity
import Data.Int
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Iteratee (Iteratee)
import qualified Data.Iteratee as I
import Data.Iteratee.ZLib
import Data.Maybe

import Data.Iteratee.ZoomCache.Utils
import Data.ZoomCache.Common
import Data.ZoomCache.Format
import Data.ZoomCache.Numeric.Delta
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

----------------------------------------------------------------------

enumCacheFilePackets :: (Functor m, MonadIO m)
                     => [IdentifyCodec]
                     -> TrackNo
                     -> I.Enumeratee ByteString [Packet] m a
enumCacheFilePackets mappings trackNo = I.joinI .
    enumCacheFileCTP mappings . I.joinI .
    I.filter (\(_,t,_) -> t == trackNo) .
    I.mapChunks (map (\(_,_,p) -> p))

enumCacheFileSummaryLevel :: (Functor m, MonadIO m)
                          => [IdentifyCodec]
                          -> Int
                          -> I.Enumeratee ByteString [ZoomSummary] m a
enumCacheFileSummaryLevel mappings level = I.joinI . enumCacheFileSummaries mappings .
                                           I.filter (\(ZoomSummary s) -> summaryLevel s == level)

enumCacheFileSummaries :: (Functor m, MonadIO m)
                       => [IdentifyCodec]
                      -> I.Enumeratee ByteString [ZoomSummary] m a
enumCacheFileSummaries mappings = I.joinI . enumCacheFileCTS mappings .
                                  I.mapChunks (map (\(_,_,s) -> s))

enumCacheFileCTP :: (Functor m, MonadIO m)
                 => [IdentifyCodec]
                 -> I.Enumeratee ByteString [(CacheFile, TrackNo, Packet)] m a
enumCacheFileCTP mappings = I.joinI . enumCacheFile mappings .
                            I.mapChunks (catMaybes . map toCTP)
    where
        toCTP :: Stream -> Maybe (CacheFile, TrackNo, Packet)
        toCTP StreamPacket{..} = Just (strmFile, strmTrack, strmPacket)
        toCTP _                = Nothing

enumCacheFileCTS :: (Functor m, MonadIO m)
                 => [IdentifyCodec]
                 -> I.Enumeratee ByteString [(CacheFile, TrackNo, ZoomSummary)] m a
enumCacheFileCTS mappings = I.joinI . enumCacheFile mappings .
                            I.mapChunks (catMaybes . map toCTS)
    where
        toCTS :: Stream -> Maybe (CacheFile, TrackNo, ZoomSummary)
        toCTS StreamSummary{..} = Just (strmFile, strmTrack, strmSummary)
        toCTS _                 = Nothing

-- | An enumeratee of a zoom-cache file, from the global header onwards.
-- The global and track headers will be transparently read, and the 
-- 'CacheFile' visible in the 'Stream' elements.
enumCacheFile :: (Functor m, MonadIO m)
              => [IdentifyCodec]
              -> I.Enumeratee ByteString [Stream] m a
enumCacheFile mappings iter = do
    fi <- iterHeaders mappings
    enumStream fi iter

-- | An enumeratee of zoom-cache data, after global and track headers
-- have been read, or if the 'CacheFile' has been acquired elsewhere.
enumStream :: (Functor m, MonadIO m)
            => CacheFile
            -> I.Enumeratee ByteString [Stream] m a
enumStream = I.unfoldConvStream go
    where
        go :: (Functor m, MonadIO m)
           => CacheFile
           -> Iteratee ByteString m (CacheFile, [Stream])
        go cf = do
            header <- I.joinI $ I.takeUpTo 8 I.stream2list
            case parseHeader (B.pack header) of
                Just PacketHeader -> do
                    (trackNo, packet) <- readPacket (cfSpecs cf)
                    return (cf, [StreamPacket cf trackNo (fromJust packet)])
                Just SummaryHeader -> do
                    (trackNo, summary) <- readSummaryBlock (cfSpecs cf)
                    return (cf, [StreamSummary cf trackNo (fromJust summary)])
                _ -> return (cf, [])

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
iterHeaders :: (Functor m, Monad m)
            => [IdentifyCodec]
            -> I.Iteratee ByteString m CacheFile
iterHeaders mappings = iterGlobal >>= go
    where
        iterGlobal :: (Functor m, Monad m)
                   => Iteratee ByteString m CacheFile
        iterGlobal = do
            header <- I.joinI $ I.takeUpTo 8 I.stream2list
            case parseHeader (B.pack header) of
                Just GlobalHeader -> mkCacheFile <$> readGlobalHeader
                _                 -> error "No global header"

        go :: (Functor m, Monad m)
           => CacheFile
           -> Iteratee ByteString m CacheFile
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

readGlobalHeader :: (Functor m, Monad m)
                 => Iteratee ByteString m Global
readGlobalHeader = do
    v <- readVersion
    n <- readInt32be
    p <- readRational64be
    b <- readRational64be
    _u <- B.pack <$> (I.joinI $ I.takeUpTo 20 I.stream2list)
    return $ Global v n p b Nothing

readTrackHeader :: (Functor m, Monad m)
                => [IdentifyCodec]
                -> Iteratee ByteString m (TrackNo, TrackSpec)
readTrackHeader mappings = do
    trackNo <- readInt32be
    trackType <- readCodec mappings
    (drType, delta, zlib) <- readFlags
    rate <- readRational64be
    byteLength <- readInt32be
    name <- B.pack <$> (I.joinI $ I.takeUpTo byteLength I.stream2list)

    return (trackNo, TrackSpec trackType delta zlib drType rate name)

------------------------------------------------------------
-- Packet, Summary reading

enumInflateZlib :: (MonadIO m) => I.Enumeratee ByteString ByteString m a
enumInflateZlib = enumInflate Zlib defaultDecompressParams

readPacket :: (Functor m, MonadIO m)
           => IntMap TrackSpec
           -> Iteratee ByteString m (TrackNo, Maybe Packet)
readPacket specs = do
    trackNo <- readInt32be
    entryTime <- TS <$> readInt64be
    exitTime <- TS <$> readInt64be
    count <- readInt32be
    byteLength <- readInt32be
    packet <- case IM.lookup trackNo specs of
        Just TrackSpec{..} -> do
            let readDTS :: (Functor m, Monad m)
                        => Iteratee ByteString m (ZoomRaw, [TimeStamp])
                readDTS = readDataTimeStamps specType specDeltaEncode specDRType count entryTime
            (d, ts) <- if specZlibCompress
                then do
                    z <- I.joinI $ enumInflateZlib I.stream2stream
                    return $ runner1 $ I.enumPure1Chunk z readDTS
                else readDTS
            return . Just $
                (Packet trackNo entryTime exitTime count d ts)
        Nothing -> do
            I.drop byteLength
            return Nothing
    return (trackNo, packet)
    where
        runner1 :: Identity (I.Iteratee s Identity c) -> c
        runner1 = runIdentity . I.run . runIdentity

        readRawCodec :: (Functor m, Monad m)
                     => Codec -> Bool -> Int
                     -> Iteratee ByteString m ZoomRaw
        readRawCodec (Codec a) delta count = ZoomRaw . f <$> replicateM count (readRawAs a)
            where
                f | delta     = deltaDecodeRaw
                  | otherwise = id

        readRawAs :: (ZoomReadable a, Functor m, Monad m)
                  => a -> Iteratee ByteString m a
        readRawAs = const readRaw

        readDataTimeStamps :: (Functor m, Monad m)
                           => Codec -> Bool -> DataRateType -> Int -> TimeStamp
                           -> Iteratee ByteString m (ZoomRaw, [TimeStamp])
        readDataTimeStamps codec delta drType count entry = do
            d <- readRawCodec codec delta count
            ts <- readTimeStamps drType count entry
            return (d, ts)

        readTimeStamps :: (Functor m, Monad m)
                       => DataRateType -> Int -> TimeStamp
                       -> Iteratee ByteString m [TimeStamp]
        readTimeStamps drType count entry = map TS <$> case drType of
            ConstantDR -> do
                return $ take count [unTS entry ..]
            VariableDR -> do
                deltaDecode <$> replicateM count readInt64be

readSummaryBlock :: (Functor m, Monad m)
                 => IntMap TrackSpec
                 -> Iteratee ByteString m (TrackNo, Maybe ZoomSummary)
readSummaryBlock specs = do
    trackNo <- readInt32be
    lvl <- readInt32be
    entryTime <- TS <$> readInt64be
    exitTime <- TS <$> readInt64be
    byteLength <- readInt32be

    summary <- case IM.lookup trackNo specs of
        Just TrackSpec{..} -> do
            sd <- readSummaryCodec specType trackNo lvl entryTime exitTime
            return $ Just sd
        Nothing -> do
            I.drop byteLength
            return Nothing
    return (trackNo, summary)
    where
        readSummaryCodec :: (Functor m, Monad m)
                         => Codec -> TrackNo -> Int -> TimeStamp -> TimeStamp
                         -> Iteratee ByteString m ZoomSummary
        readSummaryCodec (Codec a) trackNo lvl entryTime exitTime = do
            ZoomSummary <$> (Summary trackNo lvl entryTime exitTime <$> readSummaryAs a)

        readSummaryAs :: (ZoomReadable a, Functor m, Monad m)
                      => a -> Iteratee ByteString m (SummaryData a)
        readSummaryAs = const readSummary


----------------------------------------------------------------------
-- Convenience functions

-- | Map a monadic 'Stream' processing function over an entire zoom-cache file.
mapStream :: (Functor m, MonadIO m)
          => [IdentifyCodec]
          -> (Stream -> m ())
          -> Iteratee ByteString m ()
mapStream mappings = I.joinI . enumCacheFile mappings . I.mapM_
{-# INLINABLE mapStream #-}

-- | Map a monadic 'Packet' processing function over an entire zoom-cache file.
mapPackets :: (Functor m, MonadIO m)
           => [IdentifyCodec]
           -> (Packet -> m ())
           -> Iteratee ByteString m ()
mapPackets mappings f = mapStream mappings process
    where
        process StreamPacket{..} = f strmPacket
        process _                = return ()
{-# INLINABLE mapPackets #-}

-- | Map a monadic 'Summary' processing function over an entire zoom-cache file.
mapSummaries :: (Functor m, MonadIO m)
             => [IdentifyCodec]
             -> (ZoomSummary -> m ())
             -> Iteratee ByteString m ()
mapSummaries mappings f = mapStream mappings process
    where
        process StreamSummary{..} = f strmSummary
        process _                 = return ()
{-# INLINABLE mapSummaries #-}

----------------------------------------------------------------------
-- zoom-cache datatype parsers

readVersion :: (Functor m, Monad m)
            => Iteratee ByteString m Version
readVersion = Version <$> readInt16be <*> readInt16be

readCodec :: (Functor m, Monad m)
          => [IdentifyCodec]
          -> Iteratee ByteString m Codec
readCodec mappings = do
    tt <- B.pack <$> (I.joinI $ I.takeUpTo 8 I.stream2list)
    maybe (error "Unknown track type") return (parseCodec mappings tt)

parseCodec :: [IdentifyCodec] -> IdentifyCodec
parseCodec mappings h = msum . map ($ h) $ mappings

readFlags :: (Functor m, Monad m)
          => Iteratee ByteString m (DataRateType, Bool, Bool)
readFlags = do
    (n :: Int16) <- readInt16be
    let drType = case n .&. 1 of
            0 -> ConstantDR
            _ -> VariableDR
        delta = case n .&. 2 of
            0 -> False
            _ -> True
        zlib = case n .&. 4 of
            0 -> False
            _ -> True
    return (drType, delta, zlib)

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
{- |
   Module      : Data.Iteratee.ZoomCache
   Copyright   : Conrad Parker
   License     : BSD3-style (see LICENSE)

   Maintainer  : Conrad Parker <conrad@metadecks.org>
   Stability   : unstable
   Portability : unknown

   Iteratee reading of ZoomCache files.

   A typical usage, using the iteratee @iter@ to process the level 3 summaries
   from the track called \"rainfall\":

@
  I.fileDriverRandom (enumCacheFile standardIdentifiers .
      I.joinI . filterTracksByName [\"rainfall\"] .
      I.joinI . enumSummaryLevel 3 $ iter) filename
@

   Similarly, using the iteratee @rawIter@ to process the raw data from the
   track called \"rainfall\":

@
  I.fileDriverRandom (enumCacheFile standardIdentifiers .
      I.joinI . filterTracksByName [\"rainfall\"] .
      I.joinI . enumPackets $ rawIter) filename
@
-}

----------------------------------------------------------------------

module Data.Iteratee.ZoomCache (
  -- * Types
    Stream(..)

  -- * Reading zoom-cache files and ByteStrings
  , enumCacheFile
  , wholeTrackSummary

  , iterHeaders
  , enumStream
  , enumStreamTrackNo

  -- * Stream enumeratees
  , enumPackets
  , enumSummaryLevel
  , enumSummaries
  , enumCTP
  , enumCTS
  , filterTracksByName
  , filterTracks
) where

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

-- | Read the summary of an entire track.
wholeTrackSummary :: (Functor m, MonadIO m)
                  => [IdentifyCodec]
                  -> TrackNo
                  -> Iteratee ByteString m (TrackSpec, ZoomSummary)
wholeTrackSummary identifiers trackNo = I.joinI $ enumCacheFile identifiers .
    I.joinI . filterTracks [trackNo] .  I.joinI . enumCTS $ f <$> I.last
    where
        f :: (CacheFile, TrackNo, ZoomSummary) -> (TrackSpec, ZoomSummary)
        f (cf, _, zs) = (fromJust $ IM.lookup trackNo (cfSpecs cf), zs)

----------------------------------------------------------------------

-- | Filter just the raw data
enumPackets :: (Functor m, MonadIO m)
            => I.Enumeratee [Stream] [Packet] m a
enumPackets = I.joinI . enumCTP . I.mapChunks (map (\(_,_,p) -> p))

-- | Filter summaries at a particular summary level
enumSummaryLevel :: (Functor m, MonadIO m)
                 => Int
                 -> I.Enumeratee [Stream] [ZoomSummary] m a
enumSummaryLevel level =
    I.joinI . enumSummaries .
    I.filter (\(ZoomSummary s) -> summaryLevel s == level)

-- | Filter summaries at all levels
enumSummaries :: (Functor m, MonadIO m)
              => I.Enumeratee [Stream] [ZoomSummary] m a
enumSummaries = I.joinI . enumCTS .  I.mapChunks (map (\(_,_,s) -> s))

-- | Filter raw data
enumCTP :: (Functor m, MonadIO m)
        => I.Enumeratee [Stream] [(CacheFile, TrackNo, Packet)] m a
enumCTP = I.mapChunks (catMaybes . map toCTP)
    where
        toCTP :: Stream -> Maybe (CacheFile, TrackNo, Packet)
        toCTP StreamPacket{..} = Just (strmFile, strmTrack, strmPacket)
        toCTP _                = Nothing

-- | Filter summaries
enumCTS :: (Functor m, MonadIO m)
        => I.Enumeratee [Stream] [(CacheFile, TrackNo, ZoomSummary)] m a
enumCTS = I.mapChunks (catMaybes . map toCTS)
    where
        toCTS :: Stream -> Maybe (CacheFile, TrackNo, ZoomSummary)
        toCTS StreamSummary{..} = Just (strmFile, strmTrack, strmSummary)
        toCTS _                 = Nothing

-- | Filter to a given list of track names
filterTracksByName :: (Functor m, MonadIO m)
                   => CacheFile
                   -> [ByteString]
                   -> I.Enumeratee [Stream] [Stream] m a
filterTracksByName CacheFile{..} names = filterTracks tracks
    where
        tracks :: [TrackNo]
        tracks = IM.keys (IM.filter f cfSpecs)
        f :: TrackSpec -> Bool
        f ts = specName ts `elem` names

-- | Filter to a given list of track numbers
filterTracks :: (Functor m, MonadIO m)
             => [TrackNo]
             -> I.Enumeratee [Stream] [Stream] m a
filterTracks tracks = I.filter fil
    where
        fil :: Stream -> Bool
        fil StreamPacket{..}  = strmTrack `elem` tracks
        fil StreamSummary{..} = strmTrack `elem` tracks

-- | An enumeratee of a zoom-cache file, from the global header onwards.
-- The global and track headers will be transparently read, and the 
-- 'CacheFile' visible in the 'Stream' elements.
enumCacheFile :: (Functor m, MonadIO m)
              => [IdentifyCodec]
              -> I.Enumeratee ByteString [Stream] m a
enumCacheFile identifiers iter = do
    fi <- iterHeaders identifiers
    enumStream fi iter

-- | An enumeratee of zoom-cache data, after global and track headers
-- have been read, or if the 'CacheFile' has been acquired elsewhere.
-- This version skips parsing of all tracks other than the specified 'TrackNo'.
--
-- This function should only be used in applications where only one track is
-- used from a file; if you need to process multiple tracks independently then
-- give each an iteratee filtered by filterTracks or filterTracksByName, and
-- run these in parallel on the output of 'enumCacheFile' or 'enumStream'.
-- Using this function multiple times in parallel will duplicate some parsing.
enumStreamTrackNo :: (Functor m, MonadIO m)
                  => CacheFile
                  -> TrackNo
                  -> I.Enumeratee ByteString [Stream] m a
enumStreamTrackNo cf0 trackNo = I.unfoldConvStream go cf0
    where
        go :: (Functor m, MonadIO m)
           => CacheFile
           -> Iteratee ByteString m (CacheFile, [Stream])
        go cf = do
            header <- I.joinI $ I.takeUpTo 8 I.stream2list
            case parseHeader (B.pack header) of
                Just PacketHeader -> do
                    (_, packet) <- readPacketTrackNo (cfSpecs cf) trackNo
                    let res = maybe [] (\p -> [StreamPacket cf trackNo p]) packet
                    return (cf, res)
                Just SummaryHeader -> do
                    (_, summary) <- readSummaryBlockTrackNo (cfSpecs cf) trackNo
                    let res = maybe [] (\s -> [StreamSummary cf trackNo s]) summary
                    return (cf, res)
                _ -> return (cf, [])

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
iterHeaders identifiers = iterGlobal >>= go
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
                    (trackNo, spec) <- readTrackHeader identifiers
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
readTrackHeader identifiers = do
    trackNo <- readInt32be
    trackType <- readCodec identifiers
    (drType, delta, zlib) <- readFlags
    rate <- readRational64be
    byteLength <- readInt32be
    name <- B.pack <$> (I.joinI $ I.takeUpTo byteLength I.stream2list)

    return (trackNo, TrackSpec trackType delta zlib drType rate name)

------------------------------------------------------------
-- Packet, Summary reading

enumInflateZlib :: (MonadIO m) => I.Enumeratee ByteString ByteString m a
enumInflateZlib = enumInflate Zlib defaultDecompressParams

readPacketPred :: (Functor m, MonadIO m)
               => IntMap TrackSpec
               -> ((TrackNo, TimeStamp, TimeStamp) -> Bool)
               -> Iteratee ByteString m (TrackNo, Maybe Packet)
readPacketPred specs p = do
    trackNo <- readInt32be
    entryTime <- TS <$> readInt64be
    exitTime <- TS <$> readInt64be
    count <- readInt32be
    byteLength <- readInt32be
    packet <- if (p (trackNo, entryTime, exitTime))
        then do
            readPacketData specs trackNo entryTime exitTime count byteLength
        else do
            I.drop byteLength
            return Nothing
    return (trackNo, packet)

readPacketTrackNo :: (Functor m, MonadIO m)
                  => IntMap TrackSpec
                  -> TrackNo
                  -> Iteratee ByteString m (TrackNo, Maybe Packet)
readPacketTrackNo specs wantTrackNo =
    readPacketPred specs (\(trackNo, _, _) -> trackNo == wantTrackNo)

readPacket :: (Functor m, MonadIO m)
           => IntMap TrackSpec
           -> Iteratee ByteString m (TrackNo, Maybe Packet)
readPacket specs = readPacketPred specs (const True)

readPacketData :: (Functor m, MonadIO m)
               => IntMap TrackSpec
               -> TrackNo
               -> TimeStamp -> TimeStamp
               -> Int
               -> Int
               -> Iteratee ByteString m (Maybe Packet)
readPacketData specs trackNo entryTime exitTime count byteLength =
    case IM.lookup trackNo specs of
        Just TrackSpec{..} -> do
            let readDTS :: (Functor m, Monad m)
                        => Iteratee ByteString m (ZoomRaw, [TimeStamp])
                readDTS = readDataTimeStamps specType specDeltaEncode specDRType
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
    where
        runner1 :: Identity (I.Iteratee s Identity c) -> c
        runner1 = runIdentity . I.run . runIdentity

        readRawCodec :: (Functor m, Monad m)
                     => Codec -> Bool
                     -> Iteratee ByteString m ZoomRaw
        readRawCodec (Codec a) delta = ZoomRaw . f <$> replicateM count (readRawAs a)
            where
                f | delta     = deltaDecodeRaw
                  | otherwise = id

        readRawAs :: (ZoomReadable a, Functor m, Monad m)
                  => a -> Iteratee ByteString m a
        readRawAs = const readRaw

        readDataTimeStamps :: (Functor m, Monad m)
                           => Codec -> Bool -> DataRateType
                           -> Iteratee ByteString m (ZoomRaw, [TimeStamp])
        readDataTimeStamps codec delta drType = do
            d <- readRawCodec codec delta
            ts <- readTimeStamps drType
            return (d, ts)

        readTimeStamps :: (Functor m, Monad m)
                       => DataRateType
                       -> Iteratee ByteString m [TimeStamp]
        readTimeStamps drType = map TS <$> case drType of
            ConstantDR -> do
                return $ take count [unTS entryTime ..]
            VariableDR -> do
                deltaDecode <$> replicateM count readInt64be

readSummaryBlockPred :: (Functor m, Monad m)
                     => IntMap TrackSpec
                     -> ((TrackNo, Int, TimeStamp, TimeStamp) -> Bool)
                     -> Iteratee ByteString m (TrackNo, Maybe ZoomSummary)
readSummaryBlockPred specs p = do
    trackNo <- readInt32be
    lvl <- readInt32be
    entryTime <- TS <$> readInt64be
    exitTime <- TS <$> readInt64be
    byteLength <- readInt32be
    summary <- if (p (trackNo, lvl, entryTime, exitTime))
        then do
            readSummaryBlockData specs trackNo lvl entryTime exitTime byteLength
        else do
            I.drop byteLength
            return Nothing
    return (trackNo, summary)

readSummaryBlockTrackNo :: (Functor m, Monad m)
                        => IntMap TrackSpec
                        -> TrackNo
                        -> Iteratee ByteString m (TrackNo, Maybe ZoomSummary)
readSummaryBlockTrackNo specs wantTrackNo =
    readSummaryBlockPred specs (\(trackNo, _, _, _) -> trackNo == wantTrackNo)

readSummaryBlock :: (Functor m, Monad m)
                 => IntMap TrackSpec
                 -> Iteratee ByteString m (TrackNo, Maybe ZoomSummary)
readSummaryBlock specs = readSummaryBlockPred specs (const True)

readSummaryBlockData :: (Functor m, Monad m)
                     => IntMap TrackSpec
                     -> TrackNo
                     -> Int
                     -> TimeStamp -> TimeStamp
                     -> Int
                     -> Iteratee ByteString m (Maybe ZoomSummary)
readSummaryBlockData specs trackNo lvl entryTime exitTime byteLength =
    case IM.lookup trackNo specs of
        Just TrackSpec{..} -> do
            Just <$> readSummaryCodec specType
            -- sd <- readSummaryCodec specType
            -- return $ Just sd
        Nothing -> do
            I.drop byteLength
            return Nothing
    where
        readSummaryCodec :: (Functor m, Monad m)
                         => Codec
                         -> Iteratee ByteString m ZoomSummary
        readSummaryCodec (Codec a) = do
            ZoomSummary <$> (Summary trackNo lvl entryTime exitTime <$> readSummaryAs a)

        readSummaryAs :: (ZoomReadable a, Functor m, Monad m)
                      => a -> Iteratee ByteString m (SummaryData a)
        readSummaryAs = const readSummary


----------------------------------------------------------------------
-- zoom-cache datatype parsers

readVersion :: (Functor m, Monad m)
            => Iteratee ByteString m Version
readVersion = Version <$> readInt16be <*> readInt16be

readCodec :: (Functor m, Monad m)
          => [IdentifyCodec]
          -> Iteratee ByteString m Codec
readCodec identifiers = do
    tt <- B.pack <$> (I.joinI $ I.takeUpTo 8 I.stream2list)
    maybe (error "Unknown track type") return (parseCodec identifiers tt)

parseCodec :: [IdentifyCodec] -> IdentifyCodec
parseCodec identifiers h = msum . map ($ h) $ identifiers

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

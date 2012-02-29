{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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
    Block(..)
  , BlockData(..)

  -- * Reading zoom-cache files and ByteStrings
  , enumCacheFile
  , wholeTrackSummary
  , wholeTrackSummaryUTC

  , iterHeaders
  , enumBlock
  , enumBlockIncomplete
  , enumBlockTrackNo

  -- * Seeking
  , seekTimeStamp
  , seekUTCTime

  -- * Stream enumeratees
  , enumPackets
  , enumPacketsUTC
  , enumSummaryLevel
  , enumSummaries
  , enumSummaryUTCLevel
  , enumSummariesUTC
  , filterTracksByName
  , filterTracks

  -- * Low-level access to SampleOffsets
  , enumPacketSOs
  , enumSummarySOLevel
  , enumSummarySOs
  , enumCTPSO
  , enumCTSO
) where

import Control.Applicative
import Control.Monad (replicateM)
import Control.Monad.Trans (MonadIO)
import Data.Bits
import Data.ByteString (ByteString)
import Data.Functor.Identity
import Data.Int
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Iteratee (Iteratee)
import qualified Data.Iteratee as I
import qualified Data.Iteratee.Offset as OffI
import Data.Iteratee.ZLib
import Data.Maybe
import Data.Ratio
import Data.Time
import System.Posix.Types (FileOffset)

import Data.Iteratee.ZoomCache.Seek
import Data.Iteratee.ZoomCache.Utils
import Data.Offset
import Data.ZoomCache.Common
import Data.ZoomCache.Format
import Data.ZoomCache.Multichannel.Internal (supportMultichannel)
import Data.ZoomCache.Numeric.Delta
import Data.ZoomCache.Types

----------------------------------------------------------------------

data Block = Block
        { blkFile  :: CacheFile
        , blkTrack :: TrackNo
        , blkData  :: !BlockData
        }

data BlockData = BlockPacket !PacketSO | BlockSummary !ZoomSummarySO

instance Timestampable Block where
    timestamp (Block c t (BlockPacket p)) = timestamp (packetFromCTPSO (c,t,p))
    timestamp (Block c t (BlockSummary s)) = timestamp (summaryFromCTSO (c,t,s))

----------------------------------------------------------------------

-- | Read the summary of an entire track.
wholeTrackSummary :: (Functor m, MonadIO m)
                  => TrackNo
                  -> Iteratee [Offset Block] m (TrackSpec, ZoomSummary)
wholeTrackSummary trackNo = I.joinI $
    filterTracks [trackNo] .  I.joinI . enumCTSO $ f <$> I.last
    where
        f :: (CacheFile, TrackNo, ZoomSummarySO) -> (TrackSpec, ZoomSummary)
        f ctso@(cf, _, _) = (fromJust $ IM.lookup trackNo (cfSpecs cf),
                             summaryFromCTSO ctso)

-- | Read the summary of an entire track.
wholeTrackSummaryUTC :: (Functor m, MonadIO m)
                     => TrackNo
                     -> Iteratee [Offset Block] m (TrackSpec, Maybe ZoomSummaryUTC)
wholeTrackSummaryUTC trackNo = I.joinI $
    filterTracks [trackNo] .  I.joinI . enumCTSO $ f <$> I.last
    where
        f :: (CacheFile, TrackNo, ZoomSummarySO) -> (TrackSpec, Maybe ZoomSummaryUTC)
        f ctso@(cf, _, _) = (fromJust $ IM.lookup trackNo (cfSpecs cf),
                             summaryUTCFromCTSO ctso)
----------------------------------------------------------------------

-- | Filter just the raw data
enumPackets :: (Functor m, Monad m)
            => I.Enumeratee [Offset Block] [Packet] m a
enumPackets = I.joinI . enumCTPSO . I.mapChunks (map packetFromCTPSO)

-- | Convert a CTPSO triple into a Packet
packetFromCTPSO :: (CacheFile, TrackNo, PacketSO) -> Packet
packetFromCTPSO (cf, trackNo, pso) = packetFromPacketSO r pso
    where
        r = specRate . fromJust . IM.lookup trackNo . cfSpecs $ cf

----------------------------------------------------------------------

-- | Filter just the raw data, timestamped by UTC
enumPacketsUTC :: (Functor m, Monad m)
               => I.Enumeratee [Offset Block] [PacketUTC] m a
enumPacketsUTC = I.joinI . enumCTPSO . I.mapChunks (catMaybes . map packetUTCFromCTPSO)

-- | Convert a CTPSO triple into a Packet
packetUTCFromCTPSO :: (CacheFile, TrackNo, PacketSO) -> Maybe PacketUTC
packetUTCFromCTPSO (cf, trackNo, pso) = toPacket <$> base'm
    where
        toPacket base = packetUTCFromPacketSO base r pso
        r = specRate . fromJust . IM.lookup trackNo . cfSpecs $ cf
        base'm = baseUTC . cfGlobal $ cf

----------------------------------------------------------------------

-- | Filter summaries at a particular summary level
enumSummaryLevel :: (Functor m, Monad m)
                 => Int
                 -> I.Enumeratee [Offset Block] [ZoomSummary] m a
enumSummaryLevel level =
    I.joinI . enumSummaries .
    I.filter (\(ZoomSummary s) -> summaryLevel s == level)
{-# INLINE enumSummaryLevel #-}

-- | Filter summaries at all levels
enumSummaries :: (Functor m, Monad m)
              => I.Enumeratee [Offset Block] [ZoomSummary] m a
enumSummaries = I.joinI . enumCTSO .  I.mapChunks (map summaryFromCTSO)
{-# INLINE enumSummaries #-}

-- | Convert a CTSO triple into a ZoomSummary
summaryFromCTSO :: (CacheFile, TrackNo, ZoomSummarySO) -> ZoomSummary
summaryFromCTSO (cf, trackNo, (ZoomSummarySO zso)) =
    ZoomSummary (summaryFromSummarySO r zso)
    where
        r = specRate . fromJust . IM.lookup trackNo . cfSpecs $ cf

----------------------------------------------------------------------

-- | Filter summaries at a particular summary level
enumSummaryUTCLevel :: (Functor m, Monad m)
                    => Int
                    -> I.Enumeratee [Offset Block] [ZoomSummaryUTC] m a
enumSummaryUTCLevel level =
    I.joinI . enumSummariesUTC .
    I.filter (\(ZoomSummaryUTC s) -> summaryUTCLevel s == level)

-- | Filter summaries at all levels
enumSummariesUTC :: (Functor m, Monad m)
                 => I.Enumeratee [Offset Block] [ZoomSummaryUTC] m a
enumSummariesUTC = I.joinI . enumCTSO .  I.mapChunks (catMaybes . map summaryUTCFromCTSO)

-- | Convert a CTSO triple into a ZoomSummaryUTC
summaryUTCFromCTSO :: (CacheFile, TrackNo, ZoomSummarySO) -> Maybe ZoomSummaryUTC
summaryUTCFromCTSO (cf, trackNo, (ZoomSummarySO zso)) = toZS <$> base'm
    where
        toZS base = ZoomSummaryUTC (summaryUTCFromSummarySO base r zso)
        r = specRate . fromJust . IM.lookup trackNo . cfSpecs $ cf
        base'm = baseUTC . cfGlobal $ cf

----------------------------------------------------------------------

-- | Filter just the raw data
enumPacketSOs :: (Functor m, Monad m)
              => I.Enumeratee [Offset Block] [PacketSO] m a
enumPacketSOs = I.joinI . enumCTPSO . I.mapChunks (map (\(_,_,p) -> p))

-- | Filter summaries at a particular summary level
enumSummarySOLevel :: (Functor m, Monad m)
                   => Int
                   -> I.Enumeratee [Offset Block] [ZoomSummarySO] m a
enumSummarySOLevel level =
    I.joinI . enumSummarySOs .
    I.filter (\(ZoomSummarySO s) -> summarySOLevel s == level)

-- | Filter summaries at all levels
enumSummarySOs :: (Functor m, Monad m)
               => I.Enumeratee [Offset Block] [ZoomSummarySO] m a
enumSummarySOs = I.joinI . enumCTSO .  I.mapChunks (map (\(_,_,s) -> s))

-- | Filter raw data
enumCTPSO :: (Functor m, Monad m)
          => I.Enumeratee [Offset Block] [(CacheFile, TrackNo, PacketSO)] m a
enumCTPSO = I.mapChunks (catMaybes . map (toCTPSO . unwrapOffset))
    where
        toCTPSO :: Block -> Maybe (CacheFile, TrackNo, PacketSO)
        toCTPSO (Block c t (BlockPacket p)) = Just (c, t, p)
        toCTPSO _                           = Nothing

-- | Filter summaries
enumCTSO :: (Functor m, Monad m)
         => I.Enumeratee [Offset Block] [(CacheFile, TrackNo, ZoomSummarySO)] m a
enumCTSO = I.mapChunks (catMaybes . map (toCTSO . unwrapOffset))
    where
        toCTSO :: Block -> Maybe (CacheFile, TrackNo, ZoomSummarySO)
        toCTSO (Block c t (BlockSummary s)) = Just (c, t, s)
        toCTSO _                            = Nothing

-- | Filter to a given list of track names
filterTracksByName :: (Functor m, Monad m)
                   => CacheFile
                   -> [ByteString]
                   -> I.Enumeratee [Offset Block] [Offset Block] m a
filterTracksByName CacheFile{..} names = filterTracks tracks
    where
        tracks :: [TrackNo]
        tracks = IM.keys (IM.filter f cfSpecs)
        f :: TrackSpec -> Bool
        f ts = specName ts `elem` names

-- | Filter to a given list of track numbers
filterTracks :: (Functor m, Monad m)
             => [TrackNo]
             -> I.Enumeratee [Offset Block] [Offset Block] m a
filterTracks tracks = I.filter (fil . unwrapOffset)
    where
        fil :: Block -> Bool
        fil b = (blkTrack b) `elem` tracks
{-# INLINE filterTracks #-}

-- | An enumeratee of a zoom-cache file, from the global header onwards.
-- The global and track headers will be transparently read, and the 
-- 'CacheFile' visible in the 'Block' elements.
enumCacheFile :: (Functor m, MonadIO m)
              => [IdentifyCodec]
              -> I.Enumeratee (Offset ByteString) [Offset Block] m a
enumCacheFile identifiers iter = do
    fi <- iterHeaders identifiers
    enumBlock fi iter
{-# INLINE enumCacheFile #-}

-- | An enumeratee of zoom-cache data, after global and track headers
-- have been read, or if the 'CacheFile' has been acquired elsewhere.
-- This version skips parsing of all tracks other than the specified 'TrackNo'.
--
-- This function should only be used in applications where only one track is
-- used from a file; if you need to process multiple tracks independently then
-- give each an iteratee filtered by filterTracks or filterTracksByName, and
-- run these in parallel on the output of 'enumCacheFile' or 'enumBlock'.
-- Using this function multiple times in parallel will duplicate some parsing.
enumBlockTrackNo :: (Functor m, MonadIO m)
                 => CacheFile
                 -> TrackNo
                 -> I.Enumeratee (Offset ByteString) [Offset Block] m a
enumBlockTrackNo cf0 trackNo = I.unfoldConvStreamCheck I.eneeCheckIfDoneIgnore go cf0
    where
        go :: (Functor m, MonadIO m)
           => CacheFile
           -> Iteratee (Offset ByteString) m (CacheFile, [Offset Block])
        go cf = do
            o <- OffI.tell
            header <- OffI.takeBS 8
            case parseHeader header of
                Just PacketHeader -> do
                    (_, packet) <- OffI.convOffset $ readPacketTrackNo (cfSpecs cf) trackNo
                    let res = maybe [] (\p -> [Offset o (Block cf trackNo (BlockPacket p))]) packet
                    return (cf, res)
                Just SummaryHeader -> do
                    (_, summary) <- OffI.convOffset $ readSummaryBlockTrackNo (cfSpecs cf) trackNo
                    let res = maybe [] (\s -> [Offset o (Block cf trackNo (BlockSummary s))]) summary
                    return (cf, res)
                _ -> return (cf, [])

-- | An iteratee of zoom-cache which produces a singleton list of zoom-cache
-- stream, if it can.
iterBlock :: (Functor m, MonadIO m)
          => CacheFile
          -> Iteratee (Offset ByteString) m (CacheFile, [Offset Block])
iterBlock cf = {-# SCC "iterBlock" #-} do
    I.dropWhile (/= headerMarker)
    o <- OffI.tell
    header <- OffI.takeBS 8
    case parseHeader header of
        Just PacketHeader -> do
             (!trackNo, packet) <- OffI.convOffset $ readPacket (cfSpecs cf)
             return $! maybe (cf, []) (retPacket trackNo o) packet
        Just SummaryHeader -> do
             (!trackNo, summary) <- OffI.convOffset $ readSummaryBlock (cfSpecs cf)
             return $! maybe (cf, []) (retSummary trackNo o) summary
        _ -> return (cf, [])
    where
        retPacket trackNo o p = (cf', [Offset o (Block cf' trackNo (BlockPacket p))])
            where
                ms = toMS <$> (flip timeStampFromSO) (packetSOExit p) <$> r
                r = specRate <$> IM.lookup trackNo (cfSpecs cf)
                !cfO' = offs' ms o (cfOffsets cf)
                cf' = cf { cfOffsets = cfO' }
        retSummary trackNo o (ZoomSummarySO s) =
            (cf', [Offset o (Block cf' trackNo (BlockSummary (ZoomSummarySO s)))])
            where
                ms = toMS <$> (flip timeStampFromSO) (summarySOExit s) <$> r
                r = specRate <$> IM.lookup trackNo (cfSpecs cf)
                !cfO' = offs' ms o (cfOffsets cf)
                cf' = cf { cfOffsets = cfO' }

        toMS :: TimeStamp -> Int
        toMS (TS ts) = floor . (* 1000.0) $ ts

        offs' :: Maybe Int -> FileOffset -> IntMap FileOffset -> IntMap FileOffset
        offs' Nothing  _ = id
        offs' (Just x) o = IM.alter f x
            where
                f Nothing   = Just o
                f (Just o0) = Just (min o0 o)
{-# INLINE iterBlock #-}

-- | An iteratee of zoom-cache data, after global and track headers
-- have been read, or if the 'CacheFile' has been acquired elsewhere.
enumBlock :: (Functor m, MonadIO m)
          => CacheFile
          -> I.Enumeratee (Offset ByteString) [Offset Block] m a
enumBlock = I.unfoldConvStreamCheck I.eneeCheckIfDonePass iterBlock
{-# INLINE enumBlock #-}

-- | A version of enumBlock which won't fail with an EofException if the last
-- bit is incomplete (perhaps still being written to).
enumBlockIncomplete :: (Functor m, MonadIO m) =>
                        CacheFile
                     -> I.Enumeratee (Offset ByteString) [Offset Block] m a
enumBlockIncomplete = I.unfoldConvStreamCheck I.eneeCheckIfDonePass $
                      catchAndIgnoreAcc iterBlock

catchAndIgnoreAcc :: (Monad m, I.NullPoint a, I.Nullable s) =>
                     (acc -> Iteratee s m (acc, a)) -> acc
                  -> Iteratee s m (acc, a)
catchAndIgnoreAcc fi acc = I.checkErr (fi acc) >>= either onErr return
  where
    onErr _ = return (acc, I.empty)

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
            -> I.Iteratee (Offset ByteString) m CacheFile
iterHeaders identifiers = iterGlobal >>= go
    where
        iterGlobal :: (Functor m, Monad m)
                   => Iteratee (Offset ByteString) m CacheFile
        iterGlobal = do
            header <- OffI.takeBS 8
            case parseHeader header of
                Just GlobalHeader -> mkCacheFile <$> (OffI.convOffset readGlobalHeader)
                _                 -> error "No global header"

        go :: (Functor m, Monad m)
           => CacheFile
           -> Iteratee (Offset ByteString) m CacheFile
        go fi = do
            header <- OffI.takeBS 8
            case parseHeader header of
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
    checkVersion v
    n <- readInt32be
    jDay <- readIntegerVLC
    sN <- readIntegerVLC
    sD <- readIntegerVLC
    let !u = if (sD == 0)
                then Nothing
                else Just $ UTCTime (ModifiedJulianDay jDay) (fromRational (sN % sD))
    return $ Global v n u
    where
        checkVersion (Version major minor)
            | major == versionMajor && minor >= versionMinor = return ()
            | otherwise = error "Unsupported zoom-cache version"

readTrackHeader :: (Functor m, Monad m)
                => [IdentifyCodec]
                -> Iteratee (Offset ByteString) m (TrackNo, TrackSpec)
readTrackHeader identifiers = do
    trackNo <- readInt32be
    (drType, delta, zlib) <- readFlags
    rate <- readRational64be
    identLength <- readInt32be
    trackType <- maybe (error "Unknown track type") return =<< readCodecMultichannel identLength
    nameLength <- readInt32be
    name <- OffI.takeBS nameLength

    return (trackNo, TrackSpec trackType delta zlib drType rate name)
    where
        readCodecMultichannel = readCodec (supportMultichannel identifiers)

------------------------------------------------------------
-- Packet, Summary reading

enumInflateZlib :: (MonadIO m) => I.Enumeratee ByteString ByteString m a
enumInflateZlib = enumInflate Zlib defaultDecompressParams

readPacketPred :: (Functor m, MonadIO m)
               => IntMap TrackSpec
               -> ((TrackNo, SampleOffset, SampleOffset) -> Bool)
               -> Iteratee ByteString m (TrackNo, Maybe PacketSO)
readPacketPred specs p = do
    trackNo <- readInt32be
    entryTime <- SO <$> readInt64be
    exitTime <- SO <$> readInt64be
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
                  -> Iteratee ByteString m (TrackNo, Maybe PacketSO)
readPacketTrackNo specs wantTrackNo =
    readPacketPred specs (\(trackNo, _, _) -> trackNo == wantTrackNo)

readPacket :: (Functor m, MonadIO m)
           => IntMap TrackSpec
           -> Iteratee ByteString m (TrackNo, Maybe PacketSO)
readPacket specs = readPacketPred specs (const True)

readPacketData :: (Functor m, MonadIO m)
               => IntMap TrackSpec
               -> TrackNo
               -> SampleOffset -> SampleOffset
               -> Int
               -> Int
               -> Iteratee ByteString m (Maybe PacketSO)
readPacketData specs trackNo entryTime exitTime count byteLength =
    case IM.lookup trackNo specs of
        Just TrackSpec{..} -> do
            let readDTS :: (Functor m, Monad m)
                        => Iteratee ByteString m (ZoomRaw, [SampleOffset])
                readDTS = readDataSampleOffsets specType specDeltaEncode specSRType
            (d, ts) <- if specZlibCompress
                then do
                    z <- I.joinI $ enumInflateZlib I.stream2stream
                    return $ runner1 $ I.enumPure1Chunk z readDTS
                else readDTS
            return . Just $
                (PacketSO trackNo entryTime exitTime count d ts)
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

        readDataSampleOffsets :: (Functor m, Monad m)
                           => Codec -> Bool -> SampleRateType
                           -> Iteratee ByteString m (ZoomRaw, [SampleOffset])
        readDataSampleOffsets codec delta drType = do
            d <- readRawCodec codec delta
            ts <- readSampleOffsets drType
            return (d, ts)

        readSampleOffsets :: (Functor m, Monad m)
                       => SampleRateType
                       -> Iteratee ByteString m [SampleOffset]
        readSampleOffsets drType = map SO <$> case drType of
            ConstantSR -> do
                return $ take count [unSO entryTime ..]
            VariableSR -> do
                deltaDecode <$> replicateM count readInt64be

readSummaryBlockPred :: (Functor m, Monad m)
                     => IntMap TrackSpec
                     -> ((TrackNo, Int, SampleOffset, SampleOffset) -> Bool)
                     -> Iteratee ByteString m (TrackNo, Maybe ZoomSummarySO)
readSummaryBlockPred specs p = do
    trackNo <- readInt32be
    lvl <- readInt32be
    entryTime <- SO <$> readInt64be
    exitTime <- SO <$> readInt64be
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
                        -> Iteratee ByteString m (TrackNo, Maybe ZoomSummarySO)
readSummaryBlockTrackNo specs wantTrackNo =
    readSummaryBlockPred specs (\(trackNo, _, _, _) -> trackNo == wantTrackNo)

readSummaryBlock :: (Functor m, Monad m)
                 => IntMap TrackSpec
                 -> Iteratee ByteString m (TrackNo, Maybe ZoomSummarySO)
readSummaryBlock specs = readSummaryBlockPred specs (const True)

readSummaryBlockData :: (Functor m, Monad m)
                     => IntMap TrackSpec
                     -> TrackNo
                     -> Int
                     -> SampleOffset -> SampleOffset
                     -> Int
                     -> Iteratee ByteString m (Maybe ZoomSummarySO)
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
                         -> Iteratee ByteString m ZoomSummarySO
        readSummaryCodec (Codec a) = do
            ZoomSummarySO <$> (SummarySO trackNo lvl entryTime exitTime <$> readSummaryAs a)

        readSummaryAs :: (ZoomReadable a, Functor m, Monad m)
                      => a -> Iteratee ByteString m (SummaryData a)
        readSummaryAs = const readSummary
{-# INLINE readSummaryBlockData #-}


----------------------------------------------------------------------
-- zoom-cache datatype parsers

readVersion :: (Functor m, Monad m)
            => Iteratee ByteString m Version
readVersion = Version <$> readInt16be <*> readInt16be

readFlags :: (Functor m, Monad m)
          => Iteratee (Offset ByteString) m (SampleRateType, Bool, Bool)
readFlags = do
    (n :: Int16) <- readInt16be
    let drType = case n .&. 1 of
            0 -> ConstantSR
            _ -> VariableSR
        delta = case n .&. 2 of
            0 -> False
            _ -> True
        zlib = case n .&. 4 of
            0 -> False
            _ -> True
    return (drType, delta, zlib)

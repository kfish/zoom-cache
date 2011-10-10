{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}

module Data.ZoomCache.Read (
    -- * Types
      Packet(..)

    -- * Functions
    , zoomDumpFile
    , zoomDumpSummary
    , zoomDumpSummaryLevel
    , zoomReadFile
) where

import Control.Applicative ((<$>))
import Control.Monad (replicateM, when)
import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.Trans (lift, MonadIO)
import qualified Data.ByteString.Lazy as L
import Data.Default
import Data.Int
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Iteratee (Iteratee)
import qualified Data.Iteratee as I
import Data.Maybe
import Data.Ratio
import Data.Word
import Text.Printf
import Unsafe.Coerce (unsafeCoerce)

import Data.ZoomCache.Common
import Data.ZoomCache.Summary

------------------------------------------------------------

data ZoomReader m = ZoomReader
    { zrTracks :: IntMap (TrackReader m)
    , zrSpecs :: IntMap TrackSpec
    }

data TrackReader m = TrackReader
    { _trTrack      :: TrackNo
    , trReadPacket  :: Packet -> m ()
    , trReadSummary :: Summary -> m ()
    }

data PacketData = PDDouble [Double] | PDInt [Int]

data Packet = Packet
    { packetTrack :: TrackNo
    , packetEntryTime :: TimeStamp
    , packetExitTime :: TimeStamp
    , packetCount :: Int
    , packetData :: PacketData
    , packetTimeStamps :: [TimeStamp]
    }

instance Default (ZoomReader m) where
    def = ZoomReader IM.empty IM.empty

------------------------------------------------------------

addTrack :: (Monad m) => TrackNo
         -> (Packet -> m ()) -> (Summary -> m ())
         -> ZoomReader m -> ZoomReader m
addTrack trackNo pFunc sFunc zr = zr { zrTracks =  (IM.insert trackNo tr (zrTracks zr)) }
    where
        tr = TrackReader trackNo pFunc sFunc

zoomDumpFile :: [FilePath] -> IO ()
zoomDumpFile = zoomReadFile (addTrack 1 dumpData (const (return ())) def)

zoomDumpSummary :: [FilePath] -> IO ()
zoomDumpSummary = zoomReadFile (addTrack 1 (const (return ())) dumpSummary def)

zoomDumpSummaryLevel :: Int -> [FilePath] -> IO ()
zoomDumpSummaryLevel lvl = zoomReadFile (addTrack 1 (const (return ())) (dumpSummaryLevel lvl) def)

zoomReadFile :: (Functor m, MonadCatchIO m)
             => ZoomReader m
             -> [FilePath]
             -> m ()
zoomReadFile _      []       = return ()
zoomReadFile reader (path:_) = I.fileDriverRandom (zReader reader) path

zReader :: (Functor m, MonadIO m)
        => ZoomReader m
        -> Iteratee [Word8] m ()
zReader zr = do
    e <- I.isStreamFinished
    when (isNothing e) $ do
        zr' <- zReadPacket zr
        zReader zr'

dumpData :: Packet -> IO ()
dumpData p = mapM_ (\(t,d) -> printf "%s: %s\n" t d) tds
    where
        tds = zip (map (show . unTS) $ packetTimeStamps p) vals
        vals = case packetData p of
            PDDouble ds -> map show ds
            PDInt is    -> map show is

dumpSummary :: Summary -> IO ()
dumpSummary SummaryDouble{..} = do
    putStrLn $ printf "[%d - %d] lvl: %d\tentry: %.3f\texit: %.3f\tmin: %.3f\tmax: %.3f\tavg: %.3f\trms: %.3f"
        (unTS summaryEntryTime) (unTS summaryExitTime) summaryLevel
        summaryDoubleEntry summaryDoubleExit summaryDoubleMin summaryDoubleMax
        summaryAvg summaryRMS
dumpSummary SummaryInt{..} = do
    putStrLn $ printf "[%d - %d] lvl: %d\tentry: %d\texit: %df\tmin: %d\tmax: %d\tavg: %.3f\trms: %.3f"
        (unTS summaryEntryTime) (unTS summaryExitTime) summaryLevel
        summaryIntEntry summaryIntExit summaryIntMin summaryIntMax
        summaryAvg summaryRMS

dumpSummaryLevel :: Int -> Summary -> IO ()
dumpSummaryLevel level s
    | level == summaryLevel s = dumpSummary s
    | otherwise               = return ()

parseHeader :: L.ByteString -> Maybe HeaderType
parseHeader h
    | h == globalHeader  = Just GlobalHeader
    | h == trackHeader   = Just TrackHeader
    | h == packetHeader  = Just PacketHeader
    | h == summaryHeader = Just SummaryHeader
    | otherwise              = Nothing

zReadPacket :: (Functor m, MonadIO m)
            => ZoomReader m
            -> Iteratee [Word8] m (ZoomReader m)
zReadPacket zr = do
    header <- I.joinI $ I.takeUpTo 8 I.stream2list
    case parseHeader (L.pack header) of
        Just PacketHeader -> do
            trackNo <- zReadInt32
            entryTime <- TS <$> zReadInt32
            exitTime <- TS <$> zReadInt32
            byteLength <- zReadInt32
            count <- zReadInt32
            case IM.lookup trackNo (zrTracks zr) of
                Just tr -> do
                    let pFunc = trReadPacket tr
                    case IM.lookup trackNo (zrSpecs zr) of
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
                            lift $ pFunc (Packet trackNo entryTime exitTime count d ts)
                        Nothing -> I.drop byteLength
                Nothing -> I.drop byteLength
            return zr
        Just SummaryHeader -> do
            trackNo <- zReadInt32
            lvl <- zReadInt32
            entryTime <- TS <$> zReadInt32
            exitTime <- TS <$> zReadInt32
            byteLength <- zReadInt32

            case IM.lookup trackNo (zrTracks zr) of
                Just tr -> do
                    let sFunc = trReadSummary tr
                    case IM.lookup trackNo (zrSpecs zr) of
                        Just TrackSpec{..} -> do
                            case specType of
                                ZDouble -> do
                                    let n = flip div 8 byteLength
                                    [en,ex,mn,mx,avg,rms] <- replicateM n zReadFloat64be
                                    lift $ sFunc (SummaryDouble trackNo lvl entryTime exitTime en ex mn mx avg rms)
                                ZInt -> do
                                    [en,ex,mn,mx] <- replicateM 4 zReadInt32
                                    [avg,rms] <- replicateM 2 zReadFloat64be
                                    lift $ sFunc (SummaryInt trackNo lvl entryTime exitTime en ex mn mx avg rms)
                        Nothing -> I.drop byteLength
                Nothing -> I.drop byteLength
            return zr
        Just TrackHeader -> do
            (trackNo, spec) <- readTrackHeader
            return zr{ zrSpecs = IM.insert trackNo spec (zrSpecs zr) }
        _ -> return zr

readTrackHeader :: (Functor m, MonadIO m) => Iteratee [Word8] m (TrackNo, TrackSpec)
readTrackHeader = do
    trackNo <- zReadInt32
    trackType <- readTrackType
    drType <- readDataRateType

    rateNumerator <- zReadInt64
    rateDenominator <- zReadInt64
    let rate = (fromIntegral rateNumerator) % (fromIntegral rateDenominator)

    byteLength <- zReadInt32
    name <- L.pack <$> (I.joinI $ I.takeUpTo byteLength I.stream2list)

    let spec = TrackSpec trackType drType rate name

    return (trackNo, spec)

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


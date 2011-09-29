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
import Data.Word
import Text.Printf
import Unsafe.Coerce (unsafeCoerce)

import Data.ZoomCache.Common
import Data.ZoomCache.Summary

------------------------------------------------------------

data ZoomReader m = ZoomReader
    { zrTracks :: IntMap (TrackReader m)
    , zrNames :: IntMap L.ByteString
    }

data TrackReader m = TrackReader
    { _trTrack      :: ZoomTrackNo
    , trType        :: Maybe ZoomTrackType
    , trReadPacket  :: Packet -> m ()
    , trReadSummary :: Summary -> m ()
    }

data PacketData = PDDouble [Double] | PDInt [Int]

data Packet = Packet
    { packetTrack :: ZoomTrackNo
    , packetEntryTime :: Int
    , packetExitTime :: Int
    , packetLength :: Int
    , packetData :: PacketData
    }

instance Default (ZoomReader m) where
    def = ZoomReader IM.empty IM.empty

------------------------------------------------------------

addTrack :: (Monad m) => ZoomTrackNo
         -> (Packet -> m ()) -> (Summary -> m ())
         -> ZoomReader m -> ZoomReader m
addTrack trackNo pFunc sFunc zr = zr { zrTracks =  (IM.insert trackNo tr (zrTracks zr)) }
    where
        tr = TrackReader trackNo Nothing pFunc sFunc

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
dumpData p = case packetData p of
    PDDouble ds -> mapM_ print ds
    PDInt is    -> mapM_ print is

dumpSummary :: Summary -> IO ()
dumpSummary SummaryDouble{..} = do
    putStrLn $ printf "[%d - %d] lvl: %d\tentry: %.3f\texit: %.3f\tmin: %.3f\tmax: %.3f\tavg: %.3f\trms: %.3f"
        summaryEntryTime summaryExitTime summaryLevel
        summaryDoubleEntry summaryDoubleExit summaryDoubleMin summaryDoubleMax
        summaryAvg summaryRMS
dumpSummary SummaryInt{..} = do
    putStrLn $ printf "[%d - %d] lvl: %d\tentry: %d\texit: %df\tmin: %d\tmax: %d\tavg: %.3f\trms: %.3f"
        summaryEntryTime summaryExitTime summaryLevel
        summaryIntEntry summaryIntExit summaryIntMin summaryIntMax
        summaryAvg summaryRMS

dumpSummaryLevel :: Int -> Summary -> IO ()
dumpSummaryLevel level s
    | level == summaryLevel s = dumpSummary s
    | otherwise               = return ()

parseHeader :: L.ByteString -> Maybe ZoomHeaderType
parseHeader h
    | h == zoomGlobalHeader  = Just GlobalHeader
    | h == zoomTrackHeader   = Just TrackHeader
    | h == zoomPacketHeader  = Just PacketHeader
    | h == zoomSummaryHeader = Just SummaryHeader
    | otherwise              = Nothing

zReadPacket :: (Functor m, MonadIO m)
            => ZoomReader m
            -> Iteratee [Word8] m (ZoomReader m)
zReadPacket zr = do
    header <- I.joinI $ I.takeUpTo 8 I.stream2list
    case parseHeader (L.pack header) of
        Just PacketHeader -> do
            trackNo <- zReadInt32
            entryTime <- zReadInt32
            exitTime <- zReadInt32
            byteLength <- zReadInt32
            case IM.lookup trackNo (zrTracks zr) of
                Just tr -> do
                    let pFunc = trReadPacket tr
                    case trType tr of
                        Just ZoomDouble -> do
                            let n = flip div 8 byteLength
                            d <- PDDouble <$> replicateM n zReadFloat64be
                            lift $ pFunc (Packet trackNo entryTime exitTime n d)
                        Just ZoomInt -> do
                            let n = flip div 4 byteLength
                            d <- PDInt <$> replicateM n zReadInt32
                            lift $ pFunc (Packet trackNo entryTime exitTime n d)
                        Nothing -> I.drop byteLength
                Nothing -> I.drop byteLength
            return zr
        Just SummaryHeader -> do
            trackNo <- zReadInt32
            lvl <- zReadInt32
            entryTime <- zReadInt32
            exitTime <- zReadInt32
            byteLength <- zReadInt32

            case IM.lookup trackNo (zrTracks zr) of
                Just tr -> do
                    let sFunc = trReadSummary tr
                    case trType tr of
                        Just ZoomDouble -> do
                            let n = flip div 8 byteLength
                            [en,ex,mn,mx,avg,rms] <- replicateM n zReadFloat64be
                            lift $ sFunc (SummaryDouble trackNo lvl entryTime exitTime en ex mn mx avg rms)
                        Just ZoomInt -> do
                            [en,ex,mn,mx] <- replicateM 4 zReadInt32
                            [avg,rms] <- replicateM 2 zReadFloat64be
                            lift $ sFunc (SummaryInt trackNo lvl entryTime exitTime en ex mn mx avg rms)
                        Nothing -> I.drop byteLength
                Nothing -> I.drop byteLength
            return zr
        Just TrackHeader -> do
            trackNo <- zReadInt32
            trackType <- zReadTrackType
            byteLength <- zReadInt32
            name <- L.pack <$> (I.joinI $ I.takeUpTo byteLength I.stream2list)
            return zr{ zrTracks = IM.adjust (setType trackType)
                                            trackNo
                                            (zrTracks zr)
                     , zrNames = IM.insert trackNo name (zrNames zr)
                     }
        _ -> return zr
    where
        setType :: ZoomTrackType -> TrackReader m -> TrackReader m
        setType trackType tr = tr { trType = Just trackType }

zReadInt32 :: (Functor m, MonadIO m) => Iteratee [Word8] m Int
zReadInt32 = fromIntegral . u32_to_s32 <$> I.endianRead4 I.MSB
    where
        u32_to_s32 :: Word32 -> Int32
        u32_to_s32 = fromIntegral

zReadFloat64be :: (Functor m, MonadIO m) => Iteratee [Word8] m Double
zReadFloat64be = do
    n <- I.endianRead8 I.MSB
    return (unsafeCoerce n :: Double)

zReadTrackType :: (Functor m, MonadIO m) => Iteratee [Word8] m ZoomTrackType
zReadTrackType = do
    n <- zReadInt32
    case n of
        0 -> return ZoomDouble
        1 -> return ZoomInt
        _ -> error "Bad tracktype"

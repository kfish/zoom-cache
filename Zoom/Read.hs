{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}

module Zoom.Read (
      zoomDumpFile
    , zoomDumpSummary
    , zoomDumpSummaryLevel
    , zoomReadFile
) where

import Control.Applicative ((<$>))
import Control.Monad (forever, replicateM, when)
import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.Trans (lift, MonadIO)
import qualified Data.ByteString.Lazy as L
import Data.Iteratee (Iteratee)
import qualified Data.Iteratee as I
import Data.Word
import Text.Printf
import Unsafe.Coerce (unsafeCoerce)

import Zoom.Common
import Zoom.Summary

------------------------------------------------------------

data TrackReader m = TrackReader
    { trTrack       :: ZoomTrackNo
    , trReadTrack   :: Packet -> m ()
    , trReadSummary :: Summary -> m ()
    }

data Packet = Packet
    { packetTrack :: ZoomTrackNo
    , packetEntryTime :: Int
    , packetExitTime :: Int
    , packetLength :: Int
    , packetData :: [Double]
    }

zoomDumpFile :: [FilePath] -> IO ()
zoomDumpFile = zoomReadFile (TrackReader 1 dumpData (const (return ())))

zoomDumpSummary :: [FilePath] -> IO ()
zoomDumpSummary = zoomReadFile (TrackReader 1 (const (return ())) dumpSummary)

zoomDumpSummaryLevel :: Int -> [FilePath] -> IO ()
zoomDumpSummaryLevel lvl = zoomReadFile (TrackReader 1 (const (return ())) (dumpSummaryLevel lvl))

zoomReadFile :: (Functor m, MonadCatchIO m)
             => TrackReader m
             -> [FilePath]
             -> m ()
zoomReadFile _      []       = return ()
zoomReadFile reader (path:_) = I.fileDriverRandom (zReader reader) path

zReader :: (Functor m, MonadIO m)
        => TrackReader m
        -> Iteratee [Word8] m ()
zReader = forever . zReadPacket

dumpTime :: Packet -> IO ()
dumpTime Packet{..} = putStrLn $ printf "[%d - %d]" packetEntryTime packetExitTime

dumpData :: Packet -> IO ()
dumpData Packet{..} = mapM_ print packetData

dumpSummary :: Summary -> IO ()
dumpSummary Summary{..} = do
    putStrLn $ printf "[%d - %d] lvl: %d\tentry: %.3f\texit: %.3f\tmin: %.3f\tmax: %.3f\tavg: %.3f\trms: %.3f"
        summaryEntryTime summaryExitTime summaryLevel
        summaryEntry summaryExit summaryMin summaryMax summaryAvg summaryRMS

dumpSummaryLevel :: Int -> Summary -> IO ()
dumpSummaryLevel lvl s@Summary{..}
    | lvl == summaryLevel = dumpSummary s
    | otherwise           = return ()

zReadPacket :: (Functor m, MonadIO m)
            => TrackReader m
            -> Iteratee [Word8] m ()
zReadPacket (TrackReader _ pFunc sFunc) = do
    h <- I.joinI $ I.takeUpTo 8 I.stream2list -- header
    when (h == L.unpack zoomPacketHeader) $ do
        trackNo <- zReadInt32
        entryTime <- zReadInt32
        exitTime <- zReadInt32
        n <- flip div 8 <$> zReadInt32
        d <- replicateM n zReadFloat64be
        lift $ pFunc (Packet trackNo entryTime exitTime n d)
    when (h == L.unpack zoomSummaryHeader) $ do
        trackNo <- zReadInt32
        lvl <- zReadInt32
        entryTime <- zReadInt32
        exitTime <- zReadInt32
        n <- flip div 8 <$> zReadInt32
        [en,ex,mn,mx,avg,rms] <- replicateM n zReadFloat64be
        lift $ sFunc (Summary trackNo lvl entryTime exitTime en ex mn mx avg rms)

zReadInt32 :: (Functor m, MonadIO m) => Iteratee [Word8] m Int
zReadInt32 = fromIntegral <$> I.endianRead4 I.LSB

zReadFloat64be :: (Functor m, MonadIO m) => Iteratee [Word8] m Double
zReadFloat64be = do
    n <- I.endianRead8 I.MSB
    return (unsafeCoerce n :: Double)


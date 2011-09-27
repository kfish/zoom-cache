{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}

module Data.ZoomCache.Read (
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
import Data.Default
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Iteratee (Iteratee)
import qualified Data.Iteratee as I
import Data.Word
import Text.Printf
import Unsafe.Coerce (unsafeCoerce)

import Data.ZoomCache.Common
import Data.ZoomCache.Summary

------------------------------------------------------------

data ZoomReader m = ZoomReader
    { zrTracks :: IntMap (TrackReader m)
    }

data TrackReader m = TrackReader
    { trTrack       :: ZoomTrackNo
    , trReadPacket  :: Packet -> m ()
    , trReadSummary :: Summary -> m ()
    }

data Packet = Packet
    { packetTrack :: ZoomTrackNo
    , packetEntryTime :: Int
    , packetExitTime :: Int
    , packetLength :: Int
    , packetData :: [Double]
    }

instance Default (ZoomReader m) where
    def = ZoomReader IM.empty

------------------------------------------------------------

addTrack :: (Monad m) => ZoomTrackNo -> (Packet -> m ()) -> (Summary -> m ())
         -> ZoomReader m -> ZoomReader m
addTrack track pFunc sFunc zr = ZoomReader (IM.insert track tr (zrTracks zr))
    where
        tr = TrackReader track pFunc sFunc

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
            => ZoomReader m
            -> Iteratee [Word8] m ()
zReadPacket zr = do
    h <- I.joinI $ I.takeUpTo 8 I.stream2list -- header
    when (h == L.unpack zoomPacketHeader) $ do
        trackNo <- zReadInt32
        entryTime <- zReadInt32
        exitTime <- zReadInt32
        n <- flip div 8 <$> zReadInt32
        d <- replicateM n zReadFloat64be
        case IM.lookup trackNo (zrTracks zr) of
            Just tr -> do
                let pFunc = trReadPacket tr
                lift $ pFunc (Packet trackNo entryTime exitTime n d)
            Nothing -> return ()
    when (h == L.unpack zoomSummaryHeader) $ do
        trackNo <- zReadInt32
        lvl <- zReadInt32
        entryTime <- zReadInt32
        exitTime <- zReadInt32
        n <- flip div 8 <$> zReadInt32
        [en,ex,mn,mx,avg,rms] <- replicateM n zReadFloat64be
        case IM.lookup trackNo (zrTracks zr) of
            Just tr -> do
                let sFunc = trReadSummary tr
                lift $ sFunc (Summary trackNo lvl entryTime exitTime en ex mn mx avg rms)
            Nothing -> return ()

zReadInt32 :: (Functor m, MonadIO m) => Iteratee [Word8] m Int
zReadInt32 = fromIntegral <$> I.endianRead4 I.MSB

zReadFloat64be :: (Functor m, MonadIO m) => Iteratee [Word8] m Double
zReadFloat64be = do
    n <- I.endianRead8 I.MSB
    return (unsafeCoerce n :: Double)


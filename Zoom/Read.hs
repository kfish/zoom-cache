{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}

module Zoom.Read (
      zoomDumpFile
    , zoomDumpSummary
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

data Packet = Packet
    { packetTrack :: ZoomTrackNo
    , packetEntryTime :: Int
    , packetExitTime :: Int
    , packetLength :: Int
    , packetData :: [Double]
    }

zoomDumpFile :: [FilePath] -> IO ()
zoomDumpFile = zoomReadFile dumpData (const (return ()))

zoomDumpSummary :: [FilePath] -> IO ()
zoomDumpSummary = zoomReadFile (const (return ())) dumpSummary

zoomReadFile :: (Functor m, MonadCatchIO m)
             => (Packet -> m ())
             -> (Summary -> m ())
             -> [FilePath]
             -> m ()
zoomReadFile _     _     []       = return ()
zoomReadFile pFunc sFunc (path:_) = I.fileDriverRandom (zReader pFunc sFunc) path

zReader :: (Functor m, MonadIO m)
        => (Packet -> m ())
        -> (Summary -> m ())
        -> Iteratee [Word8] m ()
zReader pFunc sFunc = forever (zReadPacket pFunc sFunc)

dumpTime :: Packet -> IO ()
dumpTime Packet{..} = putStrLn $ printf "[%d - %d]" packetEntryTime packetExitTime

dumpData :: Packet -> IO ()
dumpData Packet{..} = mapM_ print packetData

dumpSummary :: Summary -> IO ()
dumpSummary Summary{..} = do
    putStrLn $ printf "[%d - %d] lvl; %d\tentry: %.3f\texit: %.3f\tmin: %.3f\tmax: %.3f\tavg: %.3f\trms: %.3f"
        summaryEntryTime summaryExitTime summaryLevel
        summaryEntry summaryExit summaryMin summaryMax summaryAvg summaryRMS

zReadPacket :: (Functor m, MonadIO m)
            => (Packet -> m ())
            -> (Summary -> m ())
            -> Iteratee [Word8] m ()
zReadPacket pFunc sFunc = do
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


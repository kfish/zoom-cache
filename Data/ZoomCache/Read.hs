{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}

module Data.ZoomCache.Read (
    -- * Functions
      zoomDumpFile
    , zoomDumpSummary
    , zoomDumpSummaryLevel
    , zoomInfoFile
) where

import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Int
import qualified Data.IntMap as IM
import qualified Data.Iteratee as I
import Data.Maybe
import Data.Ratio
import Text.Printf

import Data.Iteratee.ZoomCache
import Data.ZoomCache.Common
import Data.ZoomCache.Packet
import Data.ZoomCache.Summary

------------------------------------------------------------

zoomInfoFile :: FilePath -> IO ()
zoomInfoFile path = I.fileDriverRandom iterHeaders path >>= info

zoomDumpFile :: FilePath -> IO ()
zoomDumpFile = I.fileDriverRandom (mapPackets dumpData)

zoomDumpSummary :: FilePath -> IO ()
zoomDumpSummary = I.fileDriverRandom (mapSummaries dumpSummary)

zoomDumpSummaryLevel :: Int -> FilePath -> IO ()
zoomDumpSummaryLevel lvl = I.fileDriverRandom (mapSummaries (dumpSummaryLevel lvl))

----------------------------------------------------------------------

info :: FileInfo -> IO ()
info FileInfo{..} = do
    putStrLn . prettyGlobal $ fiGlobal
    mapM_ (putStrLn . uncurry prettyTrackSpec) . IM.assocs $ fiSpecs

prettyGlobal :: Global -> String
prettyGlobal Global{..} = unlines
    [ "Version:\t\t" ++ show vMaj ++ "." ++ show vMin
    , "No. tracks:\t\t" ++ show noTracks
    , "Presentation-time:\t" ++ ratShow presentationTime
    , "Base-time:\t\t" ++ ratShow baseTime
    , "UTC baseTime:\t\t" ++ maybe "undefined" show baseUTC
    ]
    where
        Version vMaj vMin = version

prettyTrackSpec :: TrackNo -> TrackSpec -> String
prettyTrackSpec trackNo TrackSpec{..} = unlines
    [ "Track " ++ show trackNo ++ ":"
    , "\tName:\t" ++ LC.unpack specName
    , "\tType:\t" ++ show specType
    , "\tRate:\t" ++ show specDRType ++ " " ++ ratShow specRate
    ]

ratShow :: Rational -> String
ratShow r
    | d == 0 = "0"
    | d == 1 = show n
    | otherwise = show n ++ "/" ++ show d
    where
        n = numerator r
        d = denominator r

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


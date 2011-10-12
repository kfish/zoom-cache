{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
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
-- Reading of ZoomCache files.
----------------------------------------------------------------------

module Data.ZoomCache.Read (
    -- * Functions
      zoomDumpFile
    , zoomDumpSummary
    , zoomDumpSummaryLevel
    , zoomInfoFile
) where

import Control.Applicative ((<$>))
import Data.Int
import qualified Data.IntMap as IM
import qualified Data.Iteratee as I
import Text.Printf

import Data.Iteratee.ZoomCache
import Data.ZoomCache.Common
import Data.ZoomCache.Packet
import Data.ZoomCache.Pretty
import Data.ZoomCache.Summary

------------------------------------------------------------

zoomInfoFile :: FilePath -> IO ()
zoomInfoFile path = I.fileDriverRandom iterHeaders path >>= info

zoomDumpFile :: FilePath -> IO ()
zoomDumpFile = I.fileDriverRandom (mapStream dumpData)

zoomDumpSummary :: FilePath -> IO ()
zoomDumpSummary = I.fileDriverRandom (mapSummaries dumpSummary)

zoomDumpSummaryLevel :: Int -> FilePath -> IO ()
zoomDumpSummaryLevel lvl = I.fileDriverRandom (mapSummaries (dumpSummaryLevel lvl))

----------------------------------------------------------------------

info :: CacheFile -> IO ()
info CacheFile{..} = do
    putStrLn . prettyGlobal $ cfGlobal
    mapM_ (putStrLn . uncurry prettyTrackSpec) . IM.assocs $ cfSpecs

dumpData :: Stream -> IO ()
dumpData StreamPacket{..} = mapM_ (\(t,d) -> printf "%s: %s\n" t d) tds
    where
        rate = specRate <$> IM.lookup strmTrack (cfSpecs strmFile)
        pretty = case rate of
            Just r  -> prettyTimeStamp r
            Nothing -> show . unTS
        tds = zip (map pretty (packetTimeStamps strmPacket)) vals
        vals = case packetData strmPacket of
            PDDouble ds -> map show ds
            PDInt is    -> map show is
dumpData _ = return ()

dumpSummary :: Summary -> IO ()
dumpSummary = putStrLn . prettySummary

dumpSummaryLevel :: Int -> Summary -> IO ()
dumpSummaryLevel level s
    | level == summaryLevel s = dumpSummary s
    | otherwise               = return ()


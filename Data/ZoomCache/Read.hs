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
zoomDumpSummary = I.fileDriverRandom (mapStream dumpSummary)

zoomDumpSummaryLevel :: Int -> FilePath -> IO ()
zoomDumpSummaryLevel lvl = I.fileDriverRandom (mapStream (dumpSummaryLevel lvl))

----------------------------------------------------------------------

info :: CacheFile -> IO ()
info CacheFile{..} = do
    putStrLn . prettyGlobal $ cfGlobal
    mapM_ (putStrLn . uncurry prettyTrackSpec) . IM.assocs $ cfSpecs

streamRate :: Stream -> Maybe Rational
streamRate StreamNull = Nothing
streamRate s          = specRate <$> IM.lookup (strmTrack s) (cfSpecs (strmFile s))

dumpData :: Stream -> IO ()
dumpData s@StreamPacket{..} = mapM_ (\(t,d) -> printf "%s: %s\n" t d) tds
    where
        pretty = case streamRate s of
            Just r  -> prettyTimeStamp r
            Nothing -> show . unTS
        tds = zip (map pretty (packetTimeStamps strmPacket)) vals
        vals = case packetData strmPacket of
            PDDouble ds -> map show ds
            PDInt is    -> map show is
dumpData _ = return ()

dumpSummary :: Stream -> IO ()
dumpSummary s@StreamSummary{..} = case streamRate s of
    Just r  -> putStrLn $ prettySummary r strmSummary
    Nothing -> return ()
dumpSummary _                 = return ()

dumpSummaryLevel :: Int -> Stream -> IO ()
dumpSummaryLevel level s@StreamSummary{..}
    | level == summaryLevel strmSummary = dumpSummary s
    | otherwise                         = return ()
dumpSummaryLevel _ _ = return ()


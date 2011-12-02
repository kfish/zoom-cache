{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      : Data.ZoomCache.Dump
-- Copyright   : Conrad Parker
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Conrad Parker <conrad@metadecks.org>
-- Stability   : unstable
-- Portability : unknown
--
-- Reading of ZoomCache files.
----------------------------------------------------------------------

module Data.ZoomCache.Dump (
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

import Data.ZoomCache

------------------------------------------------------------

zoomInfoFile :: [IdentifyCodec]
             -> FilePath -> IO ()
zoomInfoFile identifiers path =
    I.fileDriverRandom (iterHeaders identifiers) path >>= info

zoomDumpFile :: [IdentifyCodec] -> TrackNo -> FilePath -> IO ()
zoomDumpFile = dumpSomething dumpData

zoomDumpSummary :: [IdentifyCodec] -> TrackNo -> FilePath -> IO ()
zoomDumpSummary = dumpSomething dumpSummary

zoomDumpSummaryLevel :: Int
                     -> [IdentifyCodec] -> TrackNo -> FilePath -> IO ()
zoomDumpSummaryLevel lvl = dumpSomething (dumpSummaryLevel lvl)

dumpSomething :: (Stream -> IO ()) -> [IdentifyCodec] -> TrackNo -> FilePath -> IO ()
dumpSomething f identifiers trackNo = I.fileDriverRandom
    (I.joinI . enumCacheFile identifiers . I.joinI . filterTracks [trackNo] . I.mapM_ $ f)

----------------------------------------------------------------------

info :: CacheFile -> IO ()
info CacheFile{..} = do
    putStrLn . prettyGlobal $ cfGlobal
    mapM_ (putStrLn . uncurry prettyTrackSpec) . IM.assocs $ cfSpecs

streamRate :: Stream -> Maybe Rational
streamRate s = specRate <$> IM.lookup (strmTrack s) (cfSpecs (strmFile s))

dumpData :: Stream -> IO ()
dumpData s@StreamPacket{..} = mapM_ (\(t,d) -> printf "%s: %s\n" t d) tds
    where
        pretty = case streamRate s of
            Just r  -> prettySampleOffset r
            Nothing -> show . unSO
        tds = zip (map pretty (packetSampleOffsets strmPacket)) vals
        vals = f (packetData strmPacket)
        f (ZoomRaw a) = map prettyRaw a
dumpData _ = return ()

dumpSummary :: Stream -> IO ()
dumpSummary s@StreamSummary{..} = case streamRate s of
        Just r  -> putStrLn $ f r strmSummary
        Nothing -> return ()
    where
        f r (ZoomSummarySO a) = prettySummarySO r a
dumpSummary _ = return ()

dumpSummaryLevel :: Int -> Stream -> IO ()
dumpSummaryLevel level s@StreamSummary{..}
    | level == opLevel strmSummary = dumpSummary s
    | otherwise                    = return ()
    where opLevel (ZoomSummarySO a) = summarySOLevel a
dumpSummaryLevel _ _ = return ()


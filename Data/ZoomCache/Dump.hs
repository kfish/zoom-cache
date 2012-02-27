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
import qualified Data.IntMap as IM
import qualified Data.Iteratee as I
import qualified Data.Iteratee.IO.OffsetFd as OffI
import Data.Offset
import Text.Printf

import Data.ZoomCache

------------------------------------------------------------

zoomInfoFile :: [IdentifyCodec]
             -> FilePath -> IO ()
zoomInfoFile identifiers path =
    OffI.fileDriverRandomOBS (iterHeaders identifiers) path >>= info

zoomDumpFile :: [IdentifyCodec] -> TrackNo -> FilePath -> IO ()
zoomDumpFile = dumpSomething dumpData

zoomDumpSummary :: [IdentifyCodec] -> TrackNo -> FilePath -> IO ()
zoomDumpSummary = dumpSomething dumpSummary

zoomDumpSummaryLevel :: Int
                     -> [IdentifyCodec] -> TrackNo -> FilePath -> IO ()
zoomDumpSummaryLevel lvl = dumpSomething (dumpSummaryLevel lvl)

dumpSomething :: (Offset Block -> IO ()) -> [IdentifyCodec] -> TrackNo -> FilePath -> IO ()
dumpSomething f identifiers trackNo = OffI.fileDriverRandomOBS
    (I.joinI . enumCacheFile identifiers . I.joinI . filterTracks [trackNo] . I.mapM_ $ f)

----------------------------------------------------------------------

info :: CacheFile -> IO ()
info CacheFile{..} = do
    putStrLn . prettyGlobal $ cfGlobal
    mapM_ (putStrLn . uncurry prettyTrackSpec) . IM.assocs $ cfSpecs

blockRate :: Block -> Maybe Rational
blockRate b = specRate <$> IM.lookup (blkTrack b) (cfSpecs (blkFile b))

dumpData :: Offset Block -> IO ()
dumpData (Offset o b@(Block _ _ (BlockPacket p))) = do
    putStrLn $ printf "%07x:" (fromIntegral o :: Integer)
    mapM_ (\(t,d) -> putStrLn $ printf "%s: %s" t d) tds
    where
        pretty = case blockRate b of
            Just r  -> prettySampleOffset r
            Nothing -> show . unSO
        tds = zip (map pretty (packetSOSampleOffsets p)) vals
        vals = f (packetSOData p)
        f (ZoomRaw a) = map prettyRaw a
dumpData _ = return ()

dumpSummary :: Offset Block -> IO ()
dumpSummary (Offset o b@(Block _ _ (BlockSummary s))) = case blockRate b of
        Just r  -> putStrLn $ ox ++ f r s
        Nothing -> return ()
    where
        ox = printf "%07x: " (fromIntegral o :: Integer)
        f r (ZoomSummarySO a) = prettySummarySO r a
dumpSummary _ = return ()

dumpSummaryLevel :: Int -> Offset Block -> IO ()
dumpSummaryLevel level o@(Offset _ (Block _ _ (BlockSummary s)))
    | level == opLevel s = dumpSummary o
    | otherwise          = return ()
    where opLevel (ZoomSummarySO a) = summarySOLevel a
dumpSummaryLevel _ _ = return ()


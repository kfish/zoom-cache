{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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
-- Writing of ZoomCache files.
----------------------------------------------------------------------

module Data.ZoomCache.Write (
    -- * The ZoomWrite class
      ZoomWrite(..)

    -- * The ZoomW monad
    , ZoomW
    , withFileWrite
    , flush

    -- * ZoomWHandle IO functions
    , ZoomWHandle
    , openWrite

    -- * Watermarks
    , watermark
    , setWatermark

    -- * TrackSpec helpers
    , oneTrack
) where

import Blaze.ByteString.Builder hiding (flush)
import Control.Applicative ((<$>))
import Control.Monad.State
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Foldable as Fold
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Monoid
import System.IO

import Data.ZoomCache.Binary
import Data.ZoomCache.Common
import Data.ZoomCache.Types

------------------------------------------------------------

-- | The ZoomWrite class provides 'write', a method to write a
-- Haskell value to an open ZoomCache file.
--
class ZoomWrite t where
    -- | Write a value to an open ZoomCache file.
    write :: TrackNo -> t -> ZoomW ()

instance ZoomWrite Double where
    write = writeDouble

{-
instance ZoomWrite Int where
    write = writeInt
-}

instance ZoomWrite (TimeStamp, Double) where
    write = writeDoubleVBR

{-
instance ZoomWrite (TimeStamp, Int) where
    write = writeIntVBR
-}

------------------------------------------------------------

data ZoomWHandle a = ZoomWHandle
    { whHandle    :: Handle
    , whTrackWork :: IntMap (TrackWork a)
    , whDeferred  :: IntMap [Summary a]
    , whWriteData :: Bool
    }

data TrackWork a = TrackWork
    { twSpec      :: TrackSpec
    , twBuilder   :: Builder
    , twTSBuilder :: Builder
    , twCount     :: Int
    , twWatermark :: Int
    , twLevels    :: IntMap (Maybe (Summary a))
    , twEntryTime :: TimeStamp
    , twExitTime  :: TimeStamp
    , twData      :: SummaryWork a
    }

----------------------------------------------------------------------
-- Public API

type HurdyDurr = Double

-- | A StateT IO monad for writing a ZoomCache file
type ZoomW = StateT (ZoomWHandle HurdyDurr) IO

-- | Run a @ZoomW ()@ action on a given file handle, using the specified
-- 'TrackMap' specification
withFileWrite :: TrackMap
              -> Bool          -- ^ Whether or not to write raw data packets.
                               -- If False, only summary blocks are written.
              -> ZoomW ()
              -> FilePath
              -> IO ()
withFileWrite ztypes doRaw f path = do
    z <- openWrite ztypes doRaw path
    z' <- execStateT (f >> flush) z
    hClose (whHandle z')

-- | Force a flush of ZoomCache summary blocks to disk. It is not usually
-- necessary to call this function as summary blocks are transparently written
-- at regular intervals.
flush :: ZoomW ()
flush = do
    h <- gets whHandle
    tracks <- gets whTrackWork
    doRaw <- gets whWriteData
    when doRaw $
        liftIO $ Fold.mapM_ (L.hPut h) $ IM.mapWithKey bsFromTrack tracks
    mapM_ (uncurry flushSummary) (IM.assocs tracks)
    pending <- concat . IM.elems <$> gets whDeferred
    mapM_ writeSummary pending
    modify $ \z -> z
        { whTrackWork = IM.map flushTrack (whTrackWork z)
        , whDeferred = IM.empty
        }
    where
        flushTrack :: (ZoomSummaryWrite a)
                   => TrackWork a -> TrackWork a
        flushTrack tw = d{twLevels = twLevels tw}
            where
                d = mkTrackState (twSpec tw) (twExitTime tw) (twWatermark tw)

-- | Open a new ZoomCache file for writing, using a specified 'TrackMap'.
openWrite :: (ZoomSummaryWrite a)
          => TrackMap
          -> Bool              -- ^ Whether or not to write raw data packets.
                               -- If False, only summary blocks are written.
          -> FilePath
          -> IO (ZoomWHandle a)
openWrite trackMap doRaw path = do
    h <- openFile path WriteMode
    let global = mkGlobal (IM.size trackMap)
    writeGlobalHeader h global
    let tracks = IM.foldWithKey addTrack IM.empty trackMap
    mapM_ (uncurry (writeTrackHeader h)) (IM.assocs trackMap)
    return $ ZoomWHandle h tracks IM.empty doRaw
    where
        addTrack :: (ZoomSummaryWrite a)
                 => TrackNo -> TrackSpec
                 -> IntMap (TrackWork a)
                 -> IntMap (TrackWork a)
        addTrack trackNo spec = IM.insert trackNo trackState
            where
                trackState = mkTrackState spec (TS 0) 1024

-- | Create a track map for a stream of a given type, as track no. 1
oneTrack :: TrackType -> DataRateType -> Rational -> L.ByteString -> TrackMap
oneTrack zType drType rate name = IM.singleton 1 (TrackSpec zType drType rate name)

-- | Query the maximum number of data points to buffer for a given track before
-- forcing a flush of all buffered data and summaries.
watermark :: TrackNo -> ZoomW (Maybe Int)
watermark trackNo =  do
    track <- IM.lookup trackNo <$> gets whTrackWork
    return (twWatermark <$> track)

-- | Set the maximum number of data points to buffer for a given track before
-- forcing a flush of all buffered data and summaries.
setWatermark :: TrackNo -> Int -> ZoomW ()
setWatermark trackNo w = modifyTrack trackNo f
    where
        f :: TrackWork a -> TrackWork a
        f tw = tw { twWatermark = w }

----------------------------------------------------------------------
-- Global header

writeGlobalHeader :: Handle -> Global -> IO ()
writeGlobalHeader h = L.hPut h . toLazyByteString . fromGlobal

----------------------------------------------------------------------
-- Track header

writeTrackHeader :: Handle -> Int -> TrackSpec -> IO ()
writeTrackHeader h trackNo TrackSpec{..} = do
    L.hPut h . mconcat $
        [ trackHeader
        , toLazyByteString $ mconcat
            [ fromTrackNo trackNo
            , fromTrackType specType
            , fromDataRateType specDRType
            , fromRational64 specRate
            , encInt . LC.length $ specName
            ]
        , specName
        ]

----------------------------------------------------------------------
-- Data

incTimeStamp :: TimeStamp -> TimeStamp
incTimeStamp (TS t) = TS (t+1)

incTime :: TrackNo -> ZoomW ()
incTime trackNo = modifyTrack trackNo $ \tw -> tw
    { twEntryTime = if twCount tw == 0
                        then (incTimeStamp (twEntryTime tw))
                        else twEntryTime tw
    , twExitTime = incTimeStamp (twExitTime tw)
    }

setTime :: TrackNo -> TimeStamp -> ZoomW ()
setTime trackNo t = modifyTrack trackNo $ \tw -> tw
    { twEntryTime = if twCount tw == 0 then t else twEntryTime tw
    , twExitTime = t
    }

flushIfNeeded :: TrackNo -> ZoomW ()
flushIfNeeded trackNo = do
    zt <- IM.lookup trackNo <$> gets whTrackWork
    case zt of
        Just track -> when (flushNeeded track) flush
        Nothing -> error "no such track" -- addTrack trackNo, if no data has been written
    where
        flushNeeded :: TrackWork a -> Bool
        flushNeeded TrackWork{..} = twCount >= twWatermark

writeData :: (ZoomWrite a, a ~ HurdyDurr)
          => (a -> Builder)
          -> (Int -> TimeStamp -> a -> SummaryWork a -> SummaryWork a)
          -> TrackNo -> a -> ZoomW ()
writeData builder updater trackNo d = do
    incTime trackNo

    doRaw <- gets whWriteData
    when doRaw $
        modifyTrack trackNo $ \z -> z { twBuilder = twBuilder z <> builder d }

    modifyTrack trackNo $ \z -> z
        { twCount = twCount z + 1
        , twData = updater (twCount z) (twExitTime z) d (twData z)
        }
    flushIfNeeded trackNo

writeDataVBR :: (ZoomWrite a, ZoomSummaryWrite a, a ~ HurdyDurr)
             => (a -> Builder)
             -> (Int -> TimeStamp -> a -> SummaryWork a -> SummaryWork a)
             -> TrackNo -> (TimeStamp, a) -> ZoomW ()
writeDataVBR builder updater trackNo (t, d) = do
    setTime trackNo t

    doRaw <- gets whWriteData
    when doRaw $
        modifyTrack trackNo $ \z -> z
            { twBuilder = twBuilder z <> builder d
            , twTSBuilder = twTSBuilder z <>
                  (encInt64 .  unTS) t
            }

    modifyTrack trackNo $ \z -> z
        { twCount = twCount z + 1
        , twData = updater (twCount z) t d (twData z)
        }
    flushIfNeeded trackNo

writeDouble :: TrackNo -> Double -> ZoomW ()
writeDouble = writeData (fromWord64be . toWord64) updateSummaryData

writeDoubleVBR :: TrackNo -> (TimeStamp, Double) -> ZoomW ()
writeDoubleVBR = writeDataVBR (fromWord64be . toWord64) updateSummaryData

{- XXX KEEP THIS
writeInt :: TrackNo -> Int -> ZoomW ()
writeInt = writeData encInt updateSummaryData

writeIntVBR :: TrackNo -> (TimeStamp, Int) -> ZoomW ()
writeIntVBR = writeDataVBR encInt updateSummaryData
-}

----------------------------------------------------------------------
-- Global

mkGlobal :: Int -> Global
mkGlobal n = Global
    { version = Version versionMajor versionMinor
    , noTracks = n
    , presentationTime = 0
    , baseTime = 0
    , baseUTC = Nothing
    }

----------------------------------------------------------------------
-- TrackState

modifyTracks :: (ZoomSummaryWrite a, a ~ HurdyDurr)
             => (IntMap (TrackWork a) -> IntMap (TrackWork a)) -> ZoomW ()
modifyTracks f = modify (\z -> z { whTrackWork = f (whTrackWork z) })

modifyTrack :: (ZoomSummaryWrite a, a ~ HurdyDurr)
            => TrackNo -> (TrackWork a -> TrackWork a) -> ZoomW ()
modifyTrack trackNo f = modifyTracks (IM.adjust f trackNo)

bsFromTrack :: TrackNo -> TrackWork a -> L.ByteString
bsFromTrack trackNo TrackWork{..} = toLazyByteString $ mconcat
    [ fromLazyByteString packetHeader
    , encInt trackNo
    , encInt64 . unTS $ twEntryTime
    , encInt64 . unTS $ twExitTime
    , encInt (len twBuilder + len twTSBuilder)
    , encInt twCount
    , twBuilder
    , twTSBuilder
    ]
    where
        len = L.length . toLazyByteString

mkTrackState :: (ZoomSummaryWrite a)
             => TrackSpec -> TimeStamp -> Int -> TrackWork a
mkTrackState spec entry w = TrackWork
        { twSpec = spec
        , twBuilder = mempty
        , twTSBuilder = mempty
        , twCount = 0
        , twWatermark = w
        , twLevels = IM.empty
        , twEntryTime = entry
        , twExitTime = entry
        , twData = initSummaryWork entry -- (specType spec)
        }

----------------------------------------------------------------------
-- Summary

flushSummary :: (ZoomSummaryWrite a, a ~ HurdyDurr)
             => TrackNo -> TrackWork a -> ZoomW ()
flushSummary trackNo trackState@TrackWork{..} =
    pushSummary trackState (mkSummary trackNo trackState)

mkSummary :: (ZoomSummaryWrite a) => TrackNo -> TrackWork a -> Summary a
mkSummary trackNo TrackWork{..} = mk (specType twSpec)
    where
        mk ZDouble = Summary
            { summaryTrack = trackNo
            , summaryLevel = 1
            , summaryEntryTime = twEntryTime
            , summaryExitTime = twExitTime
            , summaryData = mkSummaryData dur twData
            }
        mk ZInt = Summary
            { summaryTrack = trackNo
            , summaryLevel = 1
            , summaryEntryTime = twEntryTime
            , summaryExitTime = twExitTime
            , summaryData = mkSummaryData dur twData
            }
        dur = fromIntegral $ (unTS twExitTime) - (unTS twEntryTime)

pushSummary :: (ZoomSummaryWrite a, a ~ HurdyDurr)
            => TrackWork a -> Summary a -> ZoomW ()
pushSummary zt s = do
    deferSummary s
    case IM.lookup (summaryLevel s) (twLevels zt) of
        Just (Just prev) -> do
            let new = (prev `appendSummary` s) { summaryLevel = summaryLevel s + 1 }
            insert Nothing
            pushSummary zt new
        _                -> do
            insert (Just s)
    where
        insert :: (ZoomSummaryWrite a, a ~ HurdyDurr)
               => Maybe (Summary a) -> ZoomW ()
        insert x = modifyTrack (summaryTrack s) (\ztt ->
            ztt { twLevels = IM.insert (summaryLevel s) x (twLevels ztt) } )

deferSummary :: (ZoomSummaryWrite a, a ~ HurdyDurr) => Summary a -> ZoomW ()
deferSummary s = do
    modify $ \z -> z
        { whDeferred = IM.alter f (summaryLevel s) (whDeferred z) }
    where
        f Nothing        = Just [s]
        f (Just pending) = Just (pending ++ [s])

writeSummary :: (ZoomSummaryWrite a) => Summary a -> ZoomW ()
writeSummary s = do
    h <- gets whHandle
    liftIO . L.hPut h . toLazyByteString . fromSummary $ s

------------------------------------------------------------

(<>) :: Monoid a => a -> a -> a
(<>) = mappend


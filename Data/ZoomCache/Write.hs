{-# LANGUAGE BangPatterns #-}
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

    -- * Instance helpers
    , writeData
    , writeDataVBR

    -- * The ZoomW monad
    , ZoomW
    , withFileWrite
    , flush

    -- * ZoomWHandle IO functions
    , ZoomWHandle
    , openWrite
    , closeWrite

    -- * Watermarks
    , watermark
    , setWatermark

    -- * TrackSpec helpers
    , mkTrackSpec
    , oneTrack
) where

import Blaze.ByteString.Builder hiding (flush)
import Codec.Compression.Zlib
import Control.Applicative ((<$>))
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.Dynamic
import qualified Data.Foldable as Fold
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Monoid
import System.IO

import Blaze.ByteString.Builder.ZoomCache
import Blaze.ByteString.Builder.ZoomCache.Internal
import Data.ZoomCache.Common
import Data.ZoomCache.Format
import Data.ZoomCache.Types

------------------------------------------------------------

-- | The ZoomWrite class provides 'write', a method to write a
-- Haskell value to an open ZoomCache file.
--
class ZoomWrite t where
    -- | Write a value to an open ZoomCache file.
    write :: TrackNo -> t -> ZoomW ()

------------------------------------------------------------

data ZoomWHandle = ZoomWHandle
    { whHandle    :: Handle
    , whTrackWork :: !(IntMap TrackWork)
    , whDeferred  :: IntMap Builder
    , whWriteData :: Bool
    }

data TrackWork = TrackWork
    { twSpec      :: TrackSpec
    , twBuilder   :: Builder
    , twTSBuilder :: Builder
    , twWriter    :: Maybe ZoomWork
    , twCount     :: {-# UNPACK #-}!Int
    , twWatermark :: {-# UNPACK #-}!Int
    , twEntryTime :: {-# UNPACK #-}!TimeStamp
    , twExitTime  :: {-# UNPACK #-}!TimeStamp
    }

----------------------------------------------------------------------
-- Public API

-- | A StateT IO monad for writing a ZoomCache file
type ZoomW = StateT ZoomWHandle IO

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
    pending <- mconcat . IM.elems <$> gets whDeferred
    liftIO . B.hPut h . toByteString $ pending
    modify $ \z -> z
        { whTrackWork = IM.map flushTrack (whTrackWork z)
        , whDeferred = IM.empty
        }
    where
        flushTrack :: TrackWork -> TrackWork
        flushTrack tw = d{twWriter = clearWork <$> (twWriter tw)}
            where
                d = mkTrackWork (twSpec tw) (twExitTime tw) (twWatermark tw)

-- | Open a new ZoomCache file for writing, using a specified 'TrackMap'.
openWrite :: TrackMap
          -> Bool              -- ^ Whether or not to write raw data packets.
                               -- If False, only summary blocks are written.
          -> FilePath
          -> IO ZoomWHandle
openWrite trackMap doRaw path = do
    h <- openFile path WriteMode
    let global = mkGlobal (IM.size trackMap)
    writeGlobalHeader h global
    let tracks = IM.foldWithKey addTrack IM.empty trackMap
    mapM_ (uncurry (writeTrackHeader h)) (IM.assocs trackMap)
    return $ ZoomWHandle h tracks IM.empty doRaw
    where
        addTrack :: TrackNo -> TrackSpec
                 -> IntMap TrackWork
                 -> IntMap TrackWork
        addTrack trackNo spec = IM.insert trackNo trackState
            where
                trackState = mkTrackWork spec (TS 0) 1024

closeWrite :: ZoomWHandle -> IO ()
closeWrite z = hClose (whHandle z)

-- | Create a track map for a stream of a given type, as track no. 1
oneTrack :: (ZoomReadable a) => a -> Bool -> Bool -> DataRateType -> Rational -> ByteString -> TrackMap
oneTrack a delta zlib !drType !rate !name = IM.singleton 1 (mkTrackSpec a delta zlib drType rate name)
{-# INLINABLE oneTrack #-}

mkTrackSpec :: (ZoomReadable a)
            => a -> Bool -> Bool -> DataRateType -> Rational -> ByteString -> TrackSpec
mkTrackSpec a = TrackSpec (Codec a)

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
        f :: TrackWork -> TrackWork
        f tw = tw { twWatermark = w }

----------------------------------------------------------------------
-- Global header

writeGlobalHeader :: Handle -> Global -> IO ()
writeGlobalHeader h = B.hPut h . toByteString . fromGlobal

----------------------------------------------------------------------
-- Track header

writeTrackHeader :: Handle -> Int -> TrackSpec -> IO ()
writeTrackHeader h trackNo TrackSpec{..} = do
    B.hPut h . mconcat $
        [ trackHeader
        , toByteString $ mconcat
            [ fromTrackNo trackNo
            , fromCodec specType
            , fromFlags specDeltaEncode specZlibCompress specDRType
            , fromRational64 specRate
            , fromIntegral32be . C.length $ specName
            ]
        , specName
        ]

----------------------------------------------------------------------
-- Data

incTimeStamp :: TimeStamp -> TimeStamp
incTimeStamp (TS t) = let t' = (t+1) in t' `seq` (TS t')

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
        flushNeeded :: TrackWork -> Bool
        flushNeeded TrackWork{..} = twCount >= twWatermark

writeData :: (Typeable a, ZoomWrite a, ZoomWritable a)
          => TrackNo -> a -> ZoomW ()
writeData trackNo d = do
    incTime trackNo

    doRaw <- gets whWriteData
    when doRaw $
        modifyTrack trackNo $ \z -> z
            { twBuilder = twBuilder z <>
                  (deltaEncodeWork (specDeltaEncode . twSpec $ z) (twWriter z) d)
            }

    modifyTrack trackNo $ \z -> let c = (twCount z) in c `seq` z
        { twCount = c + 1
        , twWriter = updateWork (twExitTime z) d (twWriter z)
        }
    flushIfNeeded trackNo

writeDataVBR :: (Typeable a, ZoomWrite a, ZoomWritable a)
             => TrackNo -> (TimeStamp, a) -> ZoomW ()
writeDataVBR trackNo (t, d) = do
    setTime trackNo t

    doRaw <- gets whWriteData
    when doRaw $
        modifyTrack trackNo $ \z -> z
            { twBuilder = twBuilder z <>
                  (deltaEncodeWork (specDeltaEncode . twSpec $ z) (twWriter z) d)
            , twTSBuilder = twTSBuilder z <> fromTimeStamp t
            }

    modifyTrack trackNo $ \z -> let c = (twCount z) in c `seq` z
        { twCount = c + 1
        , twWriter = updateWork t d (twWriter z)
        }
    flushIfNeeded trackNo

deltaEncodeWork :: (Typeable a, ZoomWritable a)
                => Bool -> Maybe ZoomWork -> a -> Builder
deltaEncodeWork False _                             d = fromRaw d
deltaEncodeWork _     (Just (ZoomWork _ (Just cw))) d =
    case (fromDynamic . toDyn $ d) of
        Just d' -> fromRaw (deltaEncode cw d')
        Nothing -> fromRaw d
deltaEncodeWork _    _                              d = fromRaw d

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

modifyTracks :: (IntMap TrackWork -> IntMap TrackWork) -> ZoomW ()
modifyTracks f = modify (\z -> z { whTrackWork = f (whTrackWork z) })

modifyTrack :: TrackNo -> (TrackWork -> TrackWork) -> ZoomW ()
modifyTrack trackNo f = modifyTracks (IM.adjust f trackNo)

bsFromTrack :: TrackNo -> TrackWork -> L.ByteString
bsFromTrack trackNo TrackWork{..} = mconcat
    [ L.pack . B.unpack $ packetHeader
    , toLazyByteString $ mconcat
        [ fromIntegral32be trackNo
        , fromTimeStamp twEntryTime
        , fromTimeStamp twExitTime
        , fromIntegral32be twCount
        , fromIntegral32be (L.length rawBS)
        ]
    , rawBS
    ]
    where
        rawBS = c $ toLazyByteString (twBuilder <> twTSBuilder)
        c | specZlibCompress twSpec = compress
          | otherwise               = id

mkTrackWork :: TrackSpec -> TimeStamp -> Int -> TrackWork
mkTrackWork !spec !entry !w = TrackWork
        { twSpec = spec
        , twBuilder = mempty
        , twTSBuilder = mempty
        , twCount = 0
        , twWatermark = w
        , twEntryTime = entry
        , twExitTime = entry
        , twWriter = Nothing
        }

----------------------------------------------------------------------
-- Working state

clearWork :: ZoomWork -> ZoomWork
clearWork (ZoomWork l _) = ZoomWork l Nothing

updateWork :: (Typeable b, ZoomWritable b)
           => TimeStamp -> b
           -> Maybe ZoomWork
           -> Maybe ZoomWork

updateWork !t !d Nothing = Just (ZoomWork IM.empty (Just cw))
    where
        cw = updateSummaryData t d (initSummaryWork t)

updateWork !t !d (Just (ZoomWork l Nothing)) =
    case cw'm of
        Just _  -> Just (ZoomWork l cw'm)
        Nothing -> Nothing
    where
        cw'm = case (fromDynamic . toDyn $ d) of
            Just d' -> Just (updateSummaryData t d' (initSummaryWork t))
            Nothing -> Nothing

updateWork !t !d (Just (ZoomWork l (Just cw))) =
    case cw'm of
        Just _  -> Just (ZoomWork l cw'm)
        Nothing -> Nothing
    where
        cw'm = case (fromDynamic . toDyn $ d) of
            Just d' -> Just (updateSummaryData t d' cw)
            Nothing -> Nothing

----------------------------------------------------------------------
-- Summary

flushSummary :: TrackNo -> TrackWork -> ZoomW ()
flushSummary trackNo TrackWork{..} = case twWriter of
    Just writer -> do
        let (writer', bs) = flushWork trackNo twEntryTime twExitTime writer
        modify $ \z -> z { whDeferred = IM.unionWith mappend (whDeferred z) bs }
        modifyTrack trackNo (\ztt -> ztt { twWriter = Just writer' } )
    _           -> return ()

flushWork :: TrackNo -> TimeStamp -> TimeStamp
          -> ZoomWork -> (ZoomWork, IntMap Builder)
flushWork _       _         _        op@(ZoomWork _ Nothing) = (op, IM.empty)
flushWork trackNo entryTime exitTime (ZoomWork l (Just cw))  =
    (ZoomWork l' (Just cw), bs)
    where
        (bs, l') = pushSummary s IM.empty l
        s = Summary
            { summaryTrack = trackNo
            , summaryLevel = 1
            , summaryEntryTime = entryTime
            , summaryExitTime = exitTime
            , summaryData = toSummaryData dur cw
            }
        dur = TSDiff $ (unTS exitTime) - (unTS entryTime)

pushSummary :: (ZoomWritable a)
            => Summary a
            -> IntMap Builder -> IntMap (Summary a -> Summary a)
            -> (IntMap Builder, IntMap (Summary a -> Summary a))
pushSummary s bs l = do
    case IM.lookup (summaryLevel s) l of
        Just g  -> pushSummary (g s) bs' cleared
        Nothing -> (bs', inserted)
    where
        bs' = IM.insert (summaryLevel s) (fromSummary s) bs
        f next = (s `appendSummary` next) { summaryLevel = summaryLevel s + 1 }
        inserted = IM.insert (summaryLevel s) f l
        cleared = IM.delete (summaryLevel s) l

-- | Append two Summaries, merging statistical summary data.
-- XXX: summaries are only compatible if tracks and levels are equal
appendSummary :: (ZoomWritable a) => Summary a -> Summary a -> Summary a
appendSummary s1 s2 = Summary
    { summaryTrack = summaryTrack s1
    , summaryLevel = summaryLevel s1
    , summaryEntryTime = summaryEntryTime s1
    , summaryExitTime = summaryExitTime s2
    , summaryData = appendSummaryData (dur s1) (summaryData s1)
                                      (dur s2) (summaryData s2)
    }
    where
        dur = summaryDuration

------------------------------------------------------------

(<>) :: Monoid a => a -> a -> a
(<>) = mappend


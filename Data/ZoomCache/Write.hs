{-# LANGUAGE CPP #-}
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
    , writeDataTS

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
) where

import Blaze.ByteString.Builder hiding (flush)
import Codec.Compression.Zlib
import Control.Applicative ((<$>))
import Control.Monad.State
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.Dynamic
import qualified Data.Foldable as Fold
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List (foldl')
import Data.Monoid
import Data.Time (UTCTime)
import System.IO

import Blaze.ByteString.Builder.ZoomCache
import Blaze.ByteString.Builder.ZoomCache.Internal
import Data.ZoomCache.Common
import Data.ZoomCache.Format
import Data.ZoomCache.Numeric.Delta
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
    , twReverseSO :: [SampleOffset]
    , twWriter    :: Maybe ZoomWork
    , twCount     :: {-# UNPACK #-}!Int
    , twWatermark :: {-# UNPACK #-}!Int
    , twEntryTime :: {-# UNPACK #-}!SampleOffset
    , twExitTime  :: {-# UNPACK #-}!SampleOffset
    }

----------------------------------------------------------------------
-- Public API

-- | A StateT IO monad for writing a ZoomCache file
type ZoomW = StateT ZoomWHandle IO

-- | Run a @ZoomW ()@ action on a given file handle, using the specified
-- 'TrackMap' specification
withFileWrite :: TrackMap
              -> Maybe UTCTime
              -> Bool          -- ^ Whether or not to write raw data packets.
                               -- If False, only summary blocks are written.
              -> ZoomW ()
              -> FilePath
              -> IO ()
withFileWrite ztypes utc doRaw f path = do
    z <- openWrite ztypes utc doRaw path
    z' <- execStateT (f >> flush >> finish) z
    hClose (whHandle z')

-- | Force a flush of ZoomCache summary blocks to disk. It is not usually
-- necessary to call this function as summary blocks are transparently written
-- at regular intervals.
flush :: ZoomW ()
flush = diskTracks flushSummarySO

-- | Write final, whole-file summary blocks.
--
-- This function flushes saved summaries at all levels, to ensure that all
-- summary levels contain data for the entire time range of the track.
--
-- In particular, the highest level of summary will contain one block for
-- the entire range of the file, and this will be the last summary block
-- in the track.
finish :: ZoomW ()
finish = diskTracks finishSummarySO

diskTracks :: (TrackNo -> TrackWork -> ZoomW ()) -> ZoomW ()
diskTracks fSummarySO = do
    h <- gets whHandle
    tracks <- gets whTrackWork
    doRaw <- gets whWriteData
    when doRaw $
        liftIO $ Fold.mapM_ (L.hPut h) $ IM.mapWithKey bsFromTrack tracks
    mapM_ (uncurry fSummarySO) (IM.assocs tracks)
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
          -> Maybe UTCTime
          -> Bool              -- ^ Whether or not to write raw data packets.
                               -- If False, only summary blocks are written.
          -> FilePath
          -> IO ZoomWHandle
openWrite trackMap utc doRaw path = do
    h <- openFile path WriteMode
    let global = mkGlobal (IM.size trackMap) utc
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
                trackState = mkTrackWork spec (SO 0) 1024

closeWrite :: ZoomWHandle -> IO ()
closeWrite z = hClose (whHandle z)

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
            , fromFlags specDeltaEncode specZlibCompress specSRType
            , fromRational64 specRate
            , fromIntegral32be . C.length $ ident
            ]
        , ident
        , toByteString . fromIntegral32be . C.length $ specName
        , specName
        ]
    where
        ident = toByteString $ fromCodec specType

----------------------------------------------------------------------
-- Data

incSampleOffset :: SampleOffset -> SampleOffset
incSampleOffset (SO t) = let t' = (t+1) in t' `seq` (SO t')

incTime :: TrackNo -> ZoomW ()
incTime trackNo = modifyTrack trackNo $ \tw -> tw
    { twEntryTime = if twCount tw == 0
                        then (incSampleOffset (twEntryTime tw))
                        else twEntryTime tw
    , twExitTime = incSampleOffset (twExitTime tw)
    }

setTime :: TrackNo -> SampleOffset -> ZoomW ()
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
             => TrackNo -> (SampleOffset, a) -> ZoomW ()
writeDataVBR trackNo (t, d) = do
    setTime trackNo t

    doRaw <- gets whWriteData
    when doRaw $
        modifyTrack trackNo $ \z -> z
            { twBuilder = twBuilder z <>
                  (deltaEncodeWork (specDeltaEncode . twSpec $ z) (twWriter z) d)
            , twReverseSO = t : twReverseSO z
            }

    modifyTrack trackNo $ \z -> let c = (twCount z) in c `seq` z
        { twCount = c + 1
        , twWriter = updateWork t d (twWriter z)
        }
    flushIfNeeded trackNo

writeDataTS :: (Typeable a, ZoomWrite a, ZoomWritable a)
            => TrackNo -> (TimeStamp, a) -> ZoomW ()
writeDataTS trackNo (TS ts, d) = do
    tw <- IM.lookup trackNo <$> gets whTrackWork
    case tw of
        Just TrackWork{..} -> do
            let so = floor (ts * fromRational (specRate twSpec))
            writeDataVBR trackNo (SO so, d)
        _ -> return ()

deltaEncodeWork :: (Typeable a, ZoomWritable a)
                => Bool -> Maybe ZoomWork -> a -> Builder
deltaEncodeWork False _                             d = fromRaw d
deltaEncodeWork _     (Just (ZoomWork _ (Just cw))) d =
    case (fromDynamic . toDyn $ d) of
        Just d' -> fromRaw (deltaEncodeRaw cw d')
        Nothing -> fromRaw d
deltaEncodeWork _    _                              d = fromRaw d

----------------------------------------------------------------------
-- Global

mkGlobal :: Int -> Maybe UTCTime -> Global
mkGlobal n utc = Global
    { version = Version versionMajor versionMinor
    , noTracks = n
    , baseUTC = utc
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
        , fromSampleOffset twEntryTime
        , fromSampleOffset twExitTime
        , fromIntegral32be twCount
        , fromIntegral32be (L.length rawBS)
        ]
    , rawBS
    ]
    where
        tsBuilder = mconcat . map fromInt64be .
                    deltaEncode . map unSO .  reverse $ twReverseSO
        rawBS = c $ toLazyByteString (twBuilder <> tsBuilder)
        c | specZlibCompress twSpec = compress
          | otherwise               = id

mkTrackWork :: TrackSpec -> SampleOffset -> Int -> TrackWork
mkTrackWork !spec !entry !w = TrackWork
        { twSpec = spec
        , twBuilder = mempty
        , twReverseSO = []
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
           => SampleOffset -> b
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
-- SummarySO

flushSummarySO :: TrackNo -> TrackWork -> ZoomW ()
flushSummarySO trackNo tw@TrackWork{..} =
    diskSummarySO (flushWork twEntryTime twExitTime) trackNo tw

finishSummarySO :: TrackNo -> TrackWork -> ZoomW ()
finishSummarySO = diskSummarySO finishWork

diskSummarySO :: (TrackNo -> ZoomWork -> (ZoomWork, IntMap Builder))
            -> TrackNo -> TrackWork -> ZoomW ()
diskSummarySO fWork trackNo TrackWork{..} = case twWriter of
    Just writer -> do
        let (writer', bs) = fWork trackNo writer
        modify $ \z -> z { whDeferred = IM.unionWith mappend (whDeferred z) bs }
        modifyTrack trackNo (\ztt -> ztt { twWriter = Just writer' } )
    _           -> return ()

finishWork :: TrackNo -> ZoomWork -> (ZoomWork, IntMap Builder)
finishWork _trackNo (ZoomWork l cw) = (ZoomWork IM.empty cw, finishLevels l)

{-

When finishing the writing of a file, we want the final, highest-level
summary block to contain data for the entire range of the file:

   1:  [ ] [ ] [ ] [ ]
        \   /   \   /
   2:    [ ]     [ ]
          \_     _/
            \   /
   3:        [ ]

However this is not usually the case -- unless, by chance, exactly 2^n level 1
summary blocks have been written.

So, we traverse all saved summary levels, and flush a summary at each level. In
order to do so we force all saved summary data to be flushed, and push that
saved data up to higher levels. In this way the contents of the final level 1
summary block are bubbled through the tree and appended to all saved summary
blocks.

   1:  [ ] [ ] [x]
        \   /  |||  Block x is propagated to the next summary level,
   2:    [s]   [x]  where it is appended to saved block s.
          \_    |
            \   /
   3:        [ ]

-}

-- Flush saved summaries at all levels, to ensure that all summary levels
-- contain data for the entire time range. In particular, the highest level
-- of summary should contain one block for the entire range of the file,
-- and this should be the last summary block in the track (as summary blocks
-- are written in order of level)
finishLevels :: (Typeable a, ZoomWritable a)
             => IntMap (SummarySO a) -> IntMap Builder
finishLevels l = snd $ foldl' propagate (Nothing, IM.empty) [1 .. fst $ IM.findMax l]
    where
        propagate (Nothing, bs) k = case IM.lookup k l of
            Nothing    -> -- Nothing propagated, nothing saved
                (Nothing, bs)
            Just saved -> -- Nothing propagated, saved to flush: propagate saved
                (Just (incLevel saved), IM.insert k (fromSummarySO saved) bs)
        propagate (Just bub, bs) k = case IM.lookup k l of
            Nothing    -> -- Something propagated to flush, nothing saved
                (Just (incLevel bub), IM.insert k (fromSummarySO bub) bs)
            Just saved -> -- Something propagated, something saved;
                          -- append these, flush and propagate
                let new = saved `appendSummarySO` bub in
                (Just (incLevel new), IM.insert k (fromSummarySO new) bs)

flushWork :: SampleOffset -> SampleOffset
          -> TrackNo -> ZoomWork -> (ZoomWork, IntMap Builder)
flushWork _         _        _       op@(ZoomWork _ Nothing) = (op, IM.empty)
flushWork entrySO exitSO trackNo (ZoomWork l (Just cw))  =
    (ZoomWork l' (Just cw), bs)
    where
        (bs, l') = pushSummarySO s IM.empty l
        s = SummarySO
            { summarySOTrack = trackNo
            , summarySOLevel = 1
            , summarySOEntry = entrySO
            , summarySOExit = exitSO
            , summarySOData = toSummaryData dur cw
            }
        dur = sampleOffsetDiff exitSO entrySO

pushSummarySO :: (ZoomWritable a)
            => SummarySO a
            -> IntMap Builder -> IntMap (SummarySO a)
            -> (IntMap Builder, IntMap (SummarySO a))
pushSummarySO s bs l = do
    case IM.lookup (summarySOLevel s) l of
        Just saved -> pushSummarySO (saved `appendSummarySO` s) bs' cleared
        Nothing    -> (bs', inserted)
    where
        bs' = IM.insert (summarySOLevel s) (fromSummarySO s) bs
        inserted = IM.insert (summarySOLevel s) (incLevel s) l
        cleared = IM.delete (summarySOLevel s) l

incLevel :: SummarySO a -> SummarySO a
incLevel s =  s { summarySOLevel = summarySOLevel s + 1 }

-- | Append two Summaries, merging statistical summary data.
-- XXX: summaries are only compatible if tracks and levels are equal
appendSummarySO :: (ZoomWritable a) => SummarySO a -> SummarySO a -> SummarySO a
appendSummarySO s1 s2 = SummarySO
    { summarySOTrack = summarySOTrack s1
    , summarySOLevel = summarySOLevel s1
    , summarySOEntry = summarySOEntry s1
    , summarySOExit = summarySOExit s2
    , summarySOData = appendSummaryData (dur s1) (summarySOData s1)
                                        (dur s2) (summarySOData s2)
    }
    where
        dur = summarySODuration

------------------------------------------------------------

#if !MIN_VERSION_base(4,5,0)

(<>) :: Monoid a => a -> a -> a
(<>) = mappend

#endif

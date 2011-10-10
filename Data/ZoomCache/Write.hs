{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
-- Writing of ZoomCache files.
----------------------------------------------------------------------

module Data.ZoomCache.Write (
    -- * The ZoomWrite class
      ZoomWrite(..)

    -- * The ZoomW monad
    , ZoomW
    , withFileWrite

    -- * ZoomWHandle IO functions
    , ZoomWHandle
    , openWrite
    , flush

    -- * Watermarks
    , watermark
    , setWatermark

    -- * TrackSpec helpers
    , oneTrack
    , oneTrackVariable
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
import Data.ZoomCache.Summary
import Numeric.FloatMinMax

------------------------------------------------------------

-- | The ZoomWrite class provides 'write', a method to write a
-- Haskell value to an open ZoomCache file.
--
class ZoomWrite t where
    -- | Write a value to an open ZoomCache file.
    write :: TrackNo -> t -> ZoomW ()

instance ZoomWrite Double where
    write = writeDouble

instance ZoomWrite Int where
    write = writeInt

instance ZoomWrite (TimeStamp, Double) where
    write = writeDoubleVBR

instance ZoomWrite (TimeStamp, Int) where
    write = writeIntVBR

------------------------------------------------------------

data ZoomWHandle = ZoomWHandle
    { whHandle    :: Handle
    , whTrackWork :: IntMap TrackWork
    , whDeferred  :: IntMap [Summary]
    , whWriteData :: Bool
    }

data TrackWork = TrackWork
    { twSpec      :: TrackSpec
    , twBuilder   :: Builder
    , twTSBuilder :: Builder
    , twCount     :: Int
    , twWatermark :: Int
    , twLevels    :: IntMap (Maybe Summary)
    , twEntryTime :: TimeStamp
    , twExitTime  :: TimeStamp
    , twData      :: ZTSData
    }

data ZTSData = ZTSDouble
    { ztsTime  :: TimeStamp
    , ztsdEntry :: Double
    , ztsdExit  :: Double
    , ztsdMin   :: Double
    , ztsdMax   :: Double
    , ztsdSum   :: Double
    , ztsSumSq  :: Double
    }
    | ZTSInt
    { ztsTime  :: TimeStamp
    , ztsiEntry :: Int
    , ztsiExit  :: Int
    , ztsiMin   :: Int
    , ztsiMax   :: Int
    , ztsiSum   :: Int
    , ztsSumSq  :: Double
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
    pending <- concat . IM.elems <$> gets whDeferred
    mapM_ writeSummary pending
    modify $ \z -> z
        { whTrackWork = IM.map flushTrack (whTrackWork z)
        , whDeferred = IM.empty
        }
    where
        flushTrack :: TrackWork -> TrackWork
        flushTrack tw = d{twLevels = twLevels tw}
            where
                d = mkTrackState (twSpec tw) (twExitTime tw) (twWatermark tw)

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
                trackState = mkTrackState spec (TS 0) 1024

-- | Create a track map for a single constant-rate stream of a given type,
-- as track no. 1
oneTrack :: TrackType -> Rational -> L.ByteString -> TrackMap
oneTrack ztype rate name = IM.singleton 1 (TrackSpec ztype ConstantDR rate name)

-- | Create a track map for a single variable-rate stream of a given type,
-- as track no. 1
oneTrackVariable :: TrackType -> L.ByteString -> TrackMap
oneTrackVariable ztype name = IM.singleton 1 (TrackSpec ztype VariableDR 0 name)

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
writeGlobalHeader h Global{..} = do
    L.hPut h . mconcat $
        [ globalHeader
        , toLazyByteString . mconcat $
            [ fromVersion version
            , encInt noTracks
            , fromRational64 presentationTime
            , fromRational64 baseTime
            ]
        , LC.pack (replicate 20 '\0') -- UTCTime
        ]

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
        flushNeeded :: TrackWork -> Bool
        flushNeeded TrackWork{..} = twCount >= twWatermark

writeData :: (ZoomWrite a)
          => (a -> Builder)
          -> (Int -> TimeStamp -> a -> ZTSData -> ZTSData)
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

writeDataVBR :: (ZoomWrite a)
             => (a -> Builder)
             -> (Int -> TimeStamp -> a -> ZTSData -> ZTSData)
             -> TrackNo -> (TimeStamp, a) -> ZoomW ()
writeDataVBR builder updater trackNo (t, d) = do
    setTime trackNo t

    doRaw <- gets whWriteData
    when doRaw $
        modifyTrack trackNo $ \z -> z
            { twBuilder = twBuilder z <> builder d
            , twTSBuilder = twTSBuilder z <>
                  (encInt .  unTS) t
            }

    modifyTrack trackNo $ \z -> z
        { twCount = twCount z + 1
        , twData = updater (twCount z) t d (twData z)
        }
    flushIfNeeded trackNo

writeDouble :: TrackNo -> Double -> ZoomW ()
writeDouble = writeData (fromWord64be . toWord64) updateZTSDouble

writeDoubleVBR :: TrackNo -> (TimeStamp, Double) -> ZoomW ()
writeDoubleVBR = writeDataVBR (fromWord64be . toWord64) updateZTSDouble

updateZTSDouble :: Int -> TimeStamp -> Double -> ZTSData -> ZTSData
updateZTSDouble count t d ZTSDouble{..} = ZTSDouble
    { ztsTime = t
    , ztsdEntry = if count == 0 then d else ztsdEntry
    , ztsdExit = d
    , ztsdMin = min ztsdMin d
    , ztsdMax = max ztsdMax d
    , ztsdSum = ztsdSum + (d * dur)
    , ztsSumSq = ztsSumSq + (d*d * dur)
    }
    where
        dur = fromIntegral $ (unTS t) - (unTS ztsTime)
updateZTSDouble _ _ _ ZTSInt{..} = error "updateZTSDouble on Int data"

writeInt :: TrackNo -> Int -> ZoomW ()
writeInt = writeData encInt updateZTSInt

writeIntVBR :: TrackNo -> (TimeStamp, Int) -> ZoomW ()
writeIntVBR = writeDataVBR encInt updateZTSInt

updateZTSInt :: Int -> TimeStamp  -> Int -> ZTSData -> ZTSData
updateZTSInt count t i ZTSInt{..} = ZTSInt
    { ztsTime = t
    , ztsiEntry = if count == 0 then i else ztsiEntry
    , ztsiExit = i
    , ztsiMin = min ztsiMin i
    , ztsiMax = max ztsiMax i
    , ztsiSum = ztsiSum + (i * dur)
    , ztsSumSq = ztsSumSq + fromIntegral (i*i * dur)
    }
    where
        dur = fromIntegral $ (unTS t) - (unTS ztsTime)
updateZTSInt _ _ _ ZTSDouble{..} = error "updateZTSInt on Double data"

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
bsFromTrack trackNo TrackWork{..} = toLazyByteString $ mconcat
    [ fromLazyByteString packetHeader
    , encInt trackNo
    , encInt . unTS $ twEntryTime
    , encInt . unTS $ twExitTime
    , encInt (len twBuilder + len twTSBuilder)
    , encInt twCount
    , twBuilder
    , twTSBuilder
    ]
    where
        len = L.length . toLazyByteString

mkTrackState :: TrackSpec -> TimeStamp -> Int -> TrackWork
mkTrackState spec entry w = TrackWork
        { twSpec = spec
        , twBuilder = mempty
        , twTSBuilder = mempty
        , twCount = 0
        , twWatermark = w
        , twLevels = IM.empty
        , twEntryTime = entry
        , twExitTime = entry
        , twData = initZTSData (specType spec)
        }
    where
        initZTSData ZDouble = ZTSDouble
            { ztsTime = entry
            , ztsdEntry = 0.0
            , ztsdExit = 0.0
            , ztsdMin = floatMax
            , ztsdMax = floatMin
            , ztsdSum = 0.0
            , ztsSumSq = 0.0
            }
        initZTSData ZInt = ZTSInt
            { ztsTime = entry
            , ztsiEntry = 0
            , ztsiExit = 0
            , ztsiMin = maxBound
            , ztsiMax = minBound
            , ztsiSum = 0
            , ztsSumSq = 0
            }

----------------------------------------------------------------------
-- Summary

flushSummary :: TrackNo -> TrackWork -> ZoomW ()
flushSummary trackNo trackState@TrackWork{..} =
    pushSummary trackState (mkSummary trackNo trackState)

mkSummary :: TrackNo -> TrackWork -> Summary
mkSummary trackNo TrackWork{..} = mk (specType twSpec)
    where
        mk ZDouble = SummaryDouble
            { summaryTrack = trackNo
            , summaryLevel = 1
            , summaryEntryTime = twEntryTime
            , summaryExitTime = twExitTime
            , summaryDoubleEntry = ztsdEntry twData
            , summaryDoubleExit = ztsdExit twData
            , summaryDoubleMin = ztsdMin twData
            , summaryDoubleMax = ztsdMax twData
            , summaryAvg = ztsdSum twData / dur
            , summaryRMS = sqrt $ ztsSumSq  twData / dur
            }
        mk ZInt = SummaryInt
            { summaryTrack = trackNo
            , summaryLevel = 1
            , summaryEntryTime = twEntryTime
            , summaryExitTime = twExitTime
            , summaryIntEntry = ztsiEntry twData
            , summaryIntExit = ztsiExit twData
            , summaryIntMin = ztsiMin twData
            , summaryIntMax = ztsiMax twData
            , summaryAvg = fromIntegral (ztsiSum twData) / dur
            , summaryRMS = sqrt $ ztsSumSq  twData / dur
            }
        dur = fromIntegral $ (unTS twExitTime) - (unTS twEntryTime)

pushSummary :: TrackWork -> Summary -> ZoomW ()
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
        insert :: Maybe Summary -> ZoomW ()
        insert x = modifyTrack (summaryTrack s) (\ztt ->
            ztt { twLevels = IM.insert (summaryLevel s) x (twLevels ztt) } )

deferSummary :: Summary -> ZoomW ()
deferSummary s = do
    modify $ \z -> z
        { whDeferred = IM.alter f (summaryLevel s) (whDeferred z) }
    where
        f Nothing        = Just [s]
        f (Just pending) = Just (pending ++ [s])

writeSummary :: Summary -> ZoomW ()
writeSummary s = do
    h <- gets whHandle
    liftIO . L.hPut h . toLazyByteString . fromSummary $ s

------------------------------------------------------------

(<>) :: Monoid a => a -> a -> a
(<>) = mappend


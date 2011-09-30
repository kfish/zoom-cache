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

    -- * Track specification
    , TrackMap
    , TrackSpec
    , oneTrack
) where

import Blaze.ByteString.Builder hiding (flush)
import Control.Applicative ((<$>))
import Control.Monad.State
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Monoid
import qualified Data.Foldable as Fold
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
    write :: TrackNo -> TimeStamp -> t -> ZoomW ()

instance ZoomWrite Double where
    write = writeDouble

instance ZoomWrite Int where
    write = writeInt

------------------------------------------------------------

data ZoomWHandle = ZoomWHandle
    { zoomHandle       :: Handle
    , zoomTracks       :: IntMap ZoomTrackState
    , zoomWritePending :: IntMap [Summary]
    }

data ZoomTrackState = ZoomTrackState
    { ztrkType      :: TrackType
    , ztrkName      :: LC.ByteString
    , ztrkBuilder   :: Builder
    , ztrkCount     :: Int
    , ztrkPending   :: Int
    , ztrkLevels    :: IntMap (Maybe Summary)
    , ztrkEntryTime :: TimeStamp
    , ztrkExitTime  :: TimeStamp
    , ztrkData      :: ZTSData
    }

data ZTSData = ZTSDouble
    { ztsdEntry :: Double
    , ztsdExit  :: Double
    , ztsdMin   :: Double
    , ztsdMax   :: Double
    , ztsdSum   :: Double
    , ztsSumSq  :: Double
    }
    | ZTSInt
    { ztsiEntry :: Int
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
withFileWrite :: TrackMap  -> ZoomW () -> FilePath -> IO ()
withFileWrite ztypes f path = do
    z <- openWrite ztypes path
    z' <- execStateT (f >> flush) z
    hClose (zoomHandle z')

-- | Force a flush of ZoomCache summary blocks to disk. It is not usually
-- necessary to call this function as summary blocks are transparently written
-- at regular intervals.
flush :: ZoomW ()
flush = do
    h <- gets zoomHandle
    tracks <- gets zoomTracks
    liftIO $ Fold.mapM_ (L.hPut h) $ IM.mapWithKey bsFromTrack tracks
    let ss = IM.mapWithKey mkSummary tracks
    Fold.mapM_ pushSummary ss
    pending <- concat . IM.elems <$> gets zoomWritePending
    mapM_ writeSummary pending
    modify $ \z -> z
        { zoomTracks = IM.map flushTrack (zoomTracks z)
        , zoomWritePending = IM.empty
        }
    where
        flushTrack :: ZoomTrackState -> ZoomTrackState
        flushTrack zt = (defTrackState (ztrkType zt)) {ztrkLevels = ztrkLevels zt}

-- | Open a new ZoomCache file for writing, using a specified 'TrackMap'.
openWrite :: TrackMap -> FilePath -> IO ZoomWHandle
openWrite ztypes path = do
    h <- openFile path WriteMode
    writeGlobalHeader h
    let tracks = IM.foldWithKey addTrack IM.empty ztypes
    mapM_ (uncurry (writeTrackHeader h)) (IM.assocs tracks)
    return $ ZoomWHandle h tracks IM.empty
    where
        addTrack :: TrackNo -> TrackSpec
                 -> IntMap ZoomTrackState
                 -> IntMap ZoomTrackState
        addTrack trackNo (TrackSpec ztype name) = IM.insert trackNo trackState
            where
                trackState = (defTrackState ztype){ztrkName = LC.pack name}

-- | A map of all track numbers to their 'TrackSpec'
type TrackMap = IntMap TrackSpec

-- | A specification of the type and name of each track
data TrackSpec = TrackSpec TrackType String

-- | Create a track map for a single stream of a given type, as track no. 1
oneTrack :: TrackType -> String -> TrackMap
oneTrack ztype name = IM.singleton 1 (TrackSpec ztype name)

----------------------------------------------------------------------
-- Global header

writeGlobalHeader :: Handle -> IO ()
writeGlobalHeader h = do
    L.hPut h . mconcat $
        [ globalHeader
        , toLazyByteString . mconcat $
            [ fromInt16be . fromIntegral $ versionMajor
            , fromInt16be . fromIntegral $ versionMinor
            , fromInt64be 0 -- Presentation time numerator
            , fromInt64be 0 -- Presentation time denominator
            , fromInt64be 0 -- Base time numerator
            , fromInt64be 0  -- Base time denominator
            ]
        , LC.pack (replicate 20 '\0')
        ]

----------------------------------------------------------------------
-- Track header

writeTrackHeader :: Handle -> Int -> ZoomTrackState -> IO ()
writeTrackHeader h trackNo ZoomTrackState{..} = do
    L.hPut h . mconcat $
        [ trackHeader
        , toLazyByteString $ mconcat
            [ fromTrackNo trackNo
            , fromTrackType ztrkType
            , fromInt32be (fromIntegral . LC.length $ ztrkName)
            ]
        , ztrkName
        ]

----------------------------------------------------------------------
-- Data

setTime :: TrackNo -> TimeStamp -> ZoomW ()
setTime trackNo t = modifyTrack trackNo $ \zt -> zt
    { ztrkEntryTime = if ztrkCount zt == 1 then t else ztrkEntryTime zt
    , ztrkExitTime = t
    }

incPending :: TrackNo -> ZoomW ()
incPending trackNo = do
    zt <- IM.lookup trackNo <$> gets zoomTracks
    case zt of
        Just track -> do
            let p = ztrkPending track
            if (p >= 1024)
                then do
                    flush
                    modifyTrack trackNo (setPending 1)
                else
                    modifyTrack trackNo (setPending (p+1))
        Nothing -> error "no such track" -- addTrack trackNo, if no data has been written
    where
        setPending :: Int -> ZoomTrackState -> ZoomTrackState
        setPending p zt = zt { ztrkPending = p }

writeDouble :: TrackNo -> TimeStamp -> Double -> ZoomW ()
writeDouble trackNo t d = do
    setTime trackNo t
    incPending trackNo
    modifyTrack trackNo $ \z -> z
        { ztrkBuilder = ztrkBuilder z <> (fromWord64be . toWord64) d
        , ztrkCount = (ztrkCount z) + 1
        , ztrkData = updateZTSDouble (ztrkCount z) d (ztrkData z)
        }

updateZTSDouble :: Int -> Double -> ZTSData -> ZTSData
updateZTSDouble count d ZTSDouble{..} = ZTSDouble
    { ztsdEntry = if count == 0 then d else ztsdEntry
    , ztsdExit = d
    , ztsdMin = min ztsdMin d
    , ztsdMax = max ztsdMax d
    , ztsdSum = ztsdSum + d
    , ztsSumSq = ztsSumSq + d*d
    }
updateZTSDouble _ _ ZTSInt{..} = error "updateZTSDouble on Int data"

writeInt :: TrackNo -> TimeStamp -> Int -> ZoomW ()
writeInt trackNo t i = do
    setTime trackNo t
    incPending trackNo
    modifyTrack trackNo $ \z -> z
        { ztrkBuilder = ztrkBuilder z <> (fromInt32be . fromIntegral) i
        , ztrkCount = (ztrkCount z) + 1
        , ztrkData = updateZTSInt (ztrkCount z) i (ztrkData z)
        }

updateZTSInt :: Int -> Int -> ZTSData -> ZTSData
updateZTSInt count i ZTSInt{..} = ZTSInt
    { ztsiEntry = if count == 0 then i else ztsiEntry
    , ztsiExit = i
    , ztsiMin = min ztsiMin i
    , ztsiMax = max ztsiMax i
    , ztsiSum = ztsiSum + i
    , ztsSumSq = ztsSumSq + fromIntegral (i*i)
    }
updateZTSInt _ _ ZTSDouble{..} = error "updateZTSInt on Double data"

----------------------------------------------------------------------
-- TrackState

modifyTracks :: (IntMap ZoomTrackState -> IntMap ZoomTrackState) -> ZoomW ()
modifyTracks f = modify (\z -> z { zoomTracks = f (zoomTracks z) })

modifyTrack :: TrackNo -> (ZoomTrackState -> ZoomTrackState) -> ZoomW ()
modifyTrack trackNo f = modifyTracks (IM.adjust f trackNo)

bsFromTrack :: TrackNo -> ZoomTrackState -> L.ByteString
bsFromTrack trackNo ZoomTrackState{..} = toLazyByteString $ mconcat
    [ fromLazyByteString packetHeader
    , encInt trackNo
    , encInt . unTS $ ztrkEntryTime
    , encInt . unTS $ ztrkExitTime
    , encInt . L.length . toLazyByteString $ ztrkBuilder
    , ztrkBuilder
    ]

defTrackState :: TrackType -> ZoomTrackState
defTrackState ZDouble = ZoomTrackState
    { ztrkType = ZDouble
    , ztrkName = ""
    , ztrkBuilder = mempty
    , ztrkCount = 0
    , ztrkPending = 1
    , ztrkLevels = IM.empty
    , ztrkEntryTime = TS 0
    , ztrkExitTime = TS 0
    , ztrkData = ZTSDouble
        { ztsdEntry = 0.0
        , ztsdExit = 0.0
        , ztsdMin = floatMax
        , ztsdMax = floatMin
        , ztsdSum = 0.0
        , ztsSumSq = 0.0
        }
    }
defTrackState ZInt = ZoomTrackState
    { ztrkType = ZInt
    , ztrkName = ""
    , ztrkBuilder = mempty
    , ztrkCount = 0
    , ztrkPending = 1
    , ztrkLevels = IM.empty
    , ztrkEntryTime = TS 0
    , ztrkExitTime = TS 0
    , ztrkData = ZTSInt
        { ztsiEntry = 0
        , ztsiExit = 0
        , ztsiMin = maxBound
        , ztsiMax = minBound
        , ztsiSum = 0
        , ztsSumSq = 0
        }
    }

----------------------------------------------------------------------
-- Summary

pushSummary :: Summary -> ZoomW ()
pushSummary s = do
    deferSummary s
    zt'm <- IM.lookup (summaryTrack s) <$> gets zoomTracks
    maybe (return ()) pushSummary' zt'm
    where
        pushSummary' :: ZoomTrackState -> ZoomW ()
        pushSummary' zt = do
            case IM.lookup (summaryLevel s) (ztrkLevels zt) of
                Just (Just prev) -> do
                    let new = (prev `appendSummary` s) { summaryLevel = summaryLevel s + 1 }
                    insert Nothing
                    pushSummary new
                _                -> do
                    insert (Just s)
            where
                insert :: Maybe Summary -> ZoomW ()
                insert x = modifyTrack (summaryTrack s) (\ztt ->
                    ztt { ztrkLevels = IM.insert (summaryLevel s) x (ztrkLevels ztt) } )

writeSummary :: Summary -> ZoomW ()
writeSummary s = do
    h <- gets zoomHandle
    liftIO . L.hPut h . toLazyByteString . fromSummary $ s

deferSummary :: Summary -> ZoomW ()
deferSummary s = do
    modify $ \z -> z
        { zoomWritePending = IM.alter f (summaryLevel s) (zoomWritePending z) }
    where
        f Nothing        = Just [s]
        f (Just pending) = Just (pending ++ [s])

mkSummary :: TrackNo -> ZoomTrackState -> Summary
mkSummary trackNo ZoomTrackState{..} = mk ztrkType
    where
        mk ZDouble = SummaryDouble
            { summaryTrack = trackNo
            , summaryLevel = 1
            , summaryEntryTime = ztrkEntryTime
            , summaryExitTime = ztrkExitTime
            , summaryDoubleEntry = ztsdEntry ztrkData
            , summaryDoubleExit = ztsdExit ztrkData
            , summaryDoubleMin = ztsdMin ztrkData
            , summaryDoubleMax = ztsdMax ztrkData
            , summaryAvg = ztsdSum ztrkData / (fromIntegral ztrkCount)
            , summaryRMS = sqrt $ ztsSumSq  ztrkData / (fromIntegral ztrkCount)
            }
        mk ZInt = SummaryInt
            { summaryTrack = trackNo
            , summaryLevel = 1
            , summaryEntryTime = ztrkEntryTime
            , summaryExitTime = ztrkExitTime
            , summaryIntEntry = ztsiEntry ztrkData
            , summaryIntExit = ztsiExit ztrkData
            , summaryIntMin = ztsiMin ztrkData
            , summaryIntMax = ztsiMax ztrkData
            , summaryAvg = fromIntegral (ztsiSum ztrkData) / (fromIntegral ztrkCount)
            , summaryRMS = sqrt $ ztsSumSq  ztrkData / (fromIntegral ztrkCount)
            }

------------------------------------------------------------

(<>) :: Monoid a => a -> a -> a
(<>) = mappend


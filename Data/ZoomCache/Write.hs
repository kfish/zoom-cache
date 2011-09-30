{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}

module Data.ZoomCache.Write (
    -- Classes
      ZoomPut(..)

    -- Types
    , Zoom
    , ZoomState(..)

    -- * State initialisation
    , oneTrack
    , openWrite
    , withFileWrite
    
    , flush
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

class ZoomPut a where
    zPut :: TrackNo -> Int -> a -> Zoom ()

instance ZoomPut Double where
    zPut = zPutDouble

instance ZoomPut Int where
    zPut = zPutInt

------------------------------------------------------------

type TrackMap = IntMap (TrackType, String)

type Zoom = StateT ZoomState IO

data ZoomState = ZoomState
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
    , ztrkEntryTime :: Int
    , ztrkExitTime  :: Int
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

openWrite :: TrackMap -> FilePath -> IO ZoomState
openWrite ztypes path = do
    h <- openFile path WriteMode
    writeGlobalHeader h
    let tracks = IM.foldWithKey addTrack IM.empty ztypes
    mapM_ (uncurry (writeTrackHeader h)) (IM.assocs tracks)
    return $ ZoomState h tracks IM.empty
    where
        addTrack :: TrackNo -> (TrackType, String) ->
                    IntMap ZoomTrackState -> IntMap ZoomTrackState
        addTrack trackNo (ztype, name) = IM.insert trackNo trackState
            where
                trackState = (defTrackState ztype){ztrkName = LC.pack name}

-- | Create a track map for a single stream of a given type, as track no. 1
oneTrack :: TrackType -> String -> TrackMap
oneTrack ztype name = IM.singleton 1 (ztype, name)

withFileWrite :: TrackMap  -> Zoom () -> FilePath -> IO ()
withFileWrite ztypes f path = do
    z <- openWrite ztypes path
    z' <- execStateT (f >> flush) z
    hClose (zoomHandle z')

flush :: Zoom ()
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

setTime :: TrackNo -> Int -> Zoom ()
setTime trackNo t = modifyTrack trackNo $ \zt -> zt
    { ztrkEntryTime = if ztrkCount zt == 1 then t else ztrkEntryTime zt
    , ztrkExitTime = t
    }

incPending :: TrackNo -> Zoom ()
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

zPutDouble :: TrackNo -> Int -> Double -> Zoom ()
zPutDouble trackNo t d = do
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

zPutInt :: TrackNo -> Int -> Int -> Zoom ()
zPutInt trackNo t i = do
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

modifyTracks :: (IntMap ZoomTrackState -> IntMap ZoomTrackState) -> Zoom ()
modifyTracks f = modify (\z -> z { zoomTracks = f (zoomTracks z) })

modifyTrack :: TrackNo -> (ZoomTrackState -> ZoomTrackState) -> Zoom ()
modifyTrack trackNo f = modifyTracks (IM.adjust f trackNo)

bsFromTrack :: TrackNo -> ZoomTrackState -> L.ByteString
bsFromTrack trackNo ZoomTrackState{..} = toLazyByteString $ mconcat
    [ fromLazyByteString packetHeader
    , encInt trackNo
    , encInt ztrkEntryTime
    , encInt ztrkExitTime
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
    , ztrkEntryTime = 0
    , ztrkExitTime = 0
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
    , ztrkEntryTime = 0
    , ztrkExitTime = 0
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

pushSummary :: Summary -> Zoom ()
pushSummary s = do
    deferSummary s
    zt'm <- IM.lookup (summaryTrack s) <$> gets zoomTracks
    maybe (return ()) pushSummary' zt'm
    where
        pushSummary' :: ZoomTrackState -> Zoom ()
        pushSummary' zt = do
            case IM.lookup (summaryLevel s) (ztrkLevels zt) of
                Just (Just prev) -> do
                    let new = (prev `appendSummary` s) { summaryLevel = summaryLevel s + 1 }
                    insert Nothing
                    pushSummary new
                _                -> do
                    insert (Just s)
            where
                insert :: Maybe Summary -> Zoom ()
                insert x = modifyTrack (summaryTrack s) (\ztt ->
                    ztt { ztrkLevels = IM.insert (summaryLevel s) x (ztrkLevels ztt) } )

writeSummary :: Summary -> Zoom ()
writeSummary s = do
    h <- gets zoomHandle
    liftIO . L.hPut h . toLazyByteString . fromSummary $ s

deferSummary :: Summary -> Zoom ()
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


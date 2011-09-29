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
import Data.Word
import System.IO
import Unsafe.Coerce (unsafeCoerce)

import Data.ZoomCache.Common
import Data.ZoomCache.Summary
import Numeric.FloatMinMax

------------------------------------------------------------

class ZoomPut a where
    zPut :: TrackNo -> Int -> a -> Zoom ()

instance ZoomPut Double where
    zPut = zoomPutDouble

instance ZoomPut Int where
    zPut = zoomPutInt

------------------------------------------------------------

(<>) :: Monoid a => a -> a -> a
(<>) = mappend

type TrackMap = IntMap (TrackType, String)

data ZoomState = ZoomState
    { zoomHandle  :: Handle
    , zoomTracks  :: IntMap ZoomTrackState
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
    , ztsdSum    :: Double
    , ztsSumSq  :: Double
    }
    | ZTSInt
    { ztsiEntry :: Int
    , ztsiExit  :: Int
    , ztsiMin   :: Int
    , ztsiMax   :: Int
    , ztsiSum    :: Int
    , ztsSumSq  :: Double
    }

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

type Zoom = StateT ZoomState IO

zoomWriteInitialHeader :: Handle -> IO ()
zoomWriteInitialHeader h = do
    let vMajor = buildInt16 versionMajor
        vMinor = buildInt16 versionMinor
        pNum = buildInt64 0 -- Presentation time numerator
        pDen = buildInt64 0 -- Presentation time denominator
        bNum = buildInt64 0 -- Base time numerator
        bDen = buildInt64 0 -- Base time denominator
        utc = LC.pack (replicate 20 '\0')
    L.hPut h globalHeader
    L.hPut h vMajor
    L.hPut h vMinor
    L.hPut h pNum
    L.hPut h pDen
    L.hPut h bNum
    L.hPut h bDen
    L.hPut h utc

buildInt16 :: Int -> L.ByteString
buildInt16 = toLazyByteString . fromInt16be . fromIntegral

buildInt32 :: Int -> L.ByteString
buildInt32 = toLazyByteString . fromInt32be . fromIntegral

buildInt64 :: Integer -> L.ByteString
buildInt64 = toLazyByteString . fromInt64be . fromIntegral

zoomWriteTrackHeader :: Handle -> Int -> ZoomTrackState -> IO ()
zoomWriteTrackHeader h trackNo ZoomTrackState{..} = do
    L.hPut h trackHeader
    L.hPut h (buildInt32 trackNo)
    L.hPut h (buildInt32 (encType ztrkType))
    L.hPut h (buildInt32 (fromIntegral . LC.length $ ztrkName))
    L.hPut h ztrkName
    where
        encType ZDouble = 0
        encType ZInt = 1

openWrite :: TrackMap -> FilePath -> IO ZoomState
openWrite ztypes path = do
    h <- openFile path WriteMode
    zoomWriteInitialHeader h
    let tracks = IM.foldWithKey addTrack IM.empty ztypes
    mapM_ (uncurry (zoomWriteTrackHeader h)) (IM.assocs tracks)
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

modifyTracks :: (IntMap ZoomTrackState -> IntMap ZoomTrackState) -> Zoom ()
modifyTracks f = modify (\z -> z { zoomTracks = f (zoomTracks z) })

modifyTrack :: TrackNo -> (ZoomTrackState -> ZoomTrackState) -> Zoom ()
modifyTrack trackNo f = modifyTracks (IM.adjust f trackNo)

zoomSetTime :: TrackNo -> Int -> Zoom ()
zoomSetTime trackNo t = modifyTrack trackNo $ \zt -> zt
    { ztrkEntryTime = if ztrkCount zt == 1 then t else ztrkEntryTime zt
    , ztrkExitTime = t
    }

zoomIncPending :: TrackNo -> Zoom ()
zoomIncPending trackNo = do
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

zoomPutDouble :: TrackNo -> Int -> Double -> Zoom ()
zoomPutDouble trackNo t d = do
    zoomSetTime trackNo t
    zoomIncPending trackNo
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

zoomPutInt :: TrackNo -> Int -> Int -> Zoom ()
zoomPutInt trackNo t i = do
    zoomSetTime trackNo t
    zoomIncPending trackNo
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

flush :: Zoom ()
flush = do
    h <- gets zoomHandle
    tracks <- gets zoomTracks
    liftIO $ Fold.mapM_ (L.hPut h) $ IM.mapWithKey zoomBuildTrack tracks
    let ss = IM.mapWithKey zoomBuildSummary tracks
    Fold.mapM_ pushSummary ss
    pending <- concat . IM.elems <$> gets zoomWritePending
    mapM_ zoomWriteSummary pending
    modify $ \z -> z
        { zoomTracks = IM.map flushTrack (zoomTracks z)
        , zoomWritePending = IM.empty
        }
    where
        flushTrack :: ZoomTrackState -> ZoomTrackState
        flushTrack zt = (defTrackState (ztrkType zt)) {ztrkLevels = ztrkLevels zt}

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

deferSummary :: Summary -> Zoom ()
deferSummary s = do
    modify $ \z -> z
        { zoomWritePending = IM.alter f (summaryLevel s) (zoomWritePending z) }
    where
        f Nothing        = Just [s]
        f (Just pending) = Just (pending ++ [s])

zoomWriteSummary :: Summary -> Zoom ()
zoomWriteSummary s = do
    h <- gets zoomHandle
    liftIO . L.hPut h . summaryToLazyByteString $ s

zoomBuildTrack :: TrackNo -> ZoomTrackState -> L.ByteString
zoomBuildTrack trackNo ZoomTrackState{..} =
    packetHeader <> no <> entryTime <> exitTime <> l <> bs
    where
        no = encInt trackNo
        entryTime = encInt ztrkEntryTime
        exitTime = encInt ztrkExitTime
        bs = toLazyByteString ztrkBuilder
        l  = encInt . L.length $ bs

zoomBuildSummary :: TrackNo -> ZoomTrackState -> Summary
zoomBuildSummary trackNo ZoomTrackState{..} = build ztrkType
    where
        build ZDouble = SummaryDouble
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
        build ZInt = SummaryInt
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

summaryToLazyByteString :: Summary -> L.ByteString
summaryToLazyByteString SummaryDouble{..} =
    summaryHeader <> no <> lvl <> entryTime <> exitTime <> l <> bs
    where
        no = encInt summaryTrack
        lvl = encInt summaryLevel
        entryTime = encInt summaryEntryTime
        exitTime = encInt summaryExitTime
        bsEn  = encDbl summaryDoubleEntry
        bsEx  = encDbl summaryDoubleExit
        bsMin = encDbl summaryDoubleMin
        bsMax = encDbl summaryDoubleMax
        bsAvg = encDbl summaryAvg
        bsRMS = encDbl summaryRMS
        bs = bsEn <> bsEx <> bsMin <> bsMax <> bsAvg <> bsRMS
        l = encInt . L.length $ bs
summaryToLazyByteString SummaryInt{..} =
    summaryHeader <> no <> lvl <> entryTime <> exitTime <> l <> bs
    where
        no = encInt summaryTrack
        lvl = encInt summaryLevel
        entryTime = encInt summaryEntryTime
        exitTime = encInt summaryExitTime
        bsEn  = encInt summaryIntEntry
        bsEx  = encInt summaryIntExit
        bsMin = encInt summaryIntMin
        bsMax = encInt summaryIntMax
        bsAvg = encDbl summaryAvg
        bsRMS = encDbl summaryRMS
        bs = bsEn <> bsEx <> bsMin <> bsMax <> bsAvg <> bsRMS
        l = encInt . L.length $ bs
    
encInt :: forall a . (Integral a) => a -> L.ByteString
encInt = toLazyByteString . fromInt32be . fromIntegral

encDbl :: Double -> L.ByteString
encDbl = toLazyByteString . fromWord64be . toWord64

toWord64 :: Double -> Word64
toWord64 = unsafeCoerce


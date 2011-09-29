{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}

module Data.ZoomCache.Write (
      Zoom
    , ZoomState(..)

    -- * State initialisation
    , zoomOpenW
    , zoomOpen1TrackW
    , zoomWithFileW
    , zoomWithFile1TrackW

    -- * Data injection
    , zoomPutInt
    , zoomPutDouble
    
    , zoomFlush
) where

import Blaze.ByteString.Builder
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

(<>) :: Monoid a => a -> a -> a
(<>) = mappend

data ZoomState = ZoomState
    { zoomHandle  :: Handle
    , zoomTracks  :: IntMap ZoomTrackState
    , zoomWritePending :: IntMap [Summary]
    }

data ZoomTrackState = ZoomTrackState
    { zoomType      :: ZoomTrackType
    , zoomBuilder   :: Builder
    , zoomCount     :: Int
    , zoomPending   :: Int
    , zoomLevels    :: IntMap (Maybe Summary)
    , zoomEntryTime :: Int
    , zoomExitTime  :: Int
    , zoomTrackData :: ZTSData
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

defTrackState :: ZoomTrackType -> ZoomTrackState
defTrackState ZoomDouble = ZoomTrackState
    { zoomType = ZoomDouble
    , zoomBuilder = mempty
    , zoomCount = 0
    , zoomPending = 1
    , zoomLevels = IM.empty
    , zoomEntryTime = 0
    , zoomExitTime = 0
    , zoomTrackData = ZTSDouble
        { ztsdEntry = 0.0
        , ztsdExit = 0.0
        , ztsdMin = floatMax
        , ztsdMax = floatMin
        , ztsdSum = 0.0
        , ztsSumSq = 0.0
        }
    }
defTrackState ZoomInt = ZoomTrackState
    { zoomType = ZoomInt
    , zoomBuilder = mempty
    , zoomCount = 0
    , zoomPending = 1
    , zoomLevels = IM.empty
    , zoomEntryTime = 0
    , zoomExitTime = 0
    , zoomTrackData = ZTSInt
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
    let vMajor = buildInt16 zoomVersionMajor
        vMinor = buildInt16 zoomVersionMinor
        pNum = buildInt64 0 -- Presentation time numerator
        pDen = buildInt64 0 -- Presentation time denominator
        bNum = buildInt64 0 -- Base time numerator
        bDen = buildInt64 0 -- Base time denominator
        utc = LC.pack (replicate 20 '\0')
    L.hPut h zoomGlobalHeader
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
    L.hPut h zoomTrackHeader
    L.hPut h (buildInt32 trackNo)
    L.hPut h (buildInt32 (encType zoomType))
    where
        encType ZoomDouble = 0
        encType ZoomInt = 1

zoomOpen1TrackW :: ZoomTrackType -> FilePath -> IO ZoomState
zoomOpen1TrackW ztype = zoomOpenW (IM.singleton 1 ztype)

zoomOpenW :: IntMap ZoomTrackType -> FilePath -> IO ZoomState
zoomOpenW ztypes path = do
    h <- openFile path WriteMode
    zoomWriteInitialHeader h
    let tracks = IM.foldWithKey addTrack IM.empty ztypes
    mapM_ (uncurry (zoomWriteTrackHeader h)) (IM.assocs tracks)
    return $ ZoomState h tracks IM.empty
    where
        addTrack :: ZoomTrackNo -> ZoomTrackType -> IntMap ZoomTrackState -> IntMap ZoomTrackState
        addTrack trackNo ztype = IM.insert trackNo (defTrackState ztype)

zoomWithFile1TrackW :: ZoomTrackType -> FilePath -> Zoom () -> IO ()
zoomWithFile1TrackW ztype = zoomWithFileW (IM.singleton 1 ztype)

zoomWithFileW :: IntMap ZoomTrackType -> FilePath -> Zoom () -> IO ()
zoomWithFileW ztypes path f = do
    z <- zoomOpenW ztypes path
    z' <- execStateT (f >> zoomFlush) z
    hClose (zoomHandle z')

modifyTracks :: (IntMap ZoomTrackState -> IntMap ZoomTrackState) -> Zoom ()
modifyTracks f = modify (\z -> z { zoomTracks = f (zoomTracks z) })

modifyTrack :: ZoomTrackNo -> (ZoomTrackState -> ZoomTrackState) -> Zoom ()
modifyTrack trackNo f = modifyTracks (IM.adjust f trackNo)

zoomSetTime :: ZoomTrackNo -> Int -> Zoom ()
zoomSetTime trackNo t = modifyTrack trackNo $ \zt -> zt
    { zoomEntryTime = if zoomCount zt == 1 then t else zoomEntryTime zt
    , zoomExitTime = t
    }

zoomIncPending :: ZoomTrackNo -> Zoom ()
zoomIncPending trackNo = do
    zt <- IM.lookup trackNo <$> gets zoomTracks
    case zt of
        Just track -> do
            let p = zoomPending track
            if (p >= 1024)
                then do
                    zoomFlush
                    modifyTrack trackNo (setPending 1)
                else
                    modifyTrack trackNo (setPending (p+1))
        Nothing -> error "no such track" -- addTrack trackNo, if no data has been written
    where
        setPending :: Int -> ZoomTrackState -> ZoomTrackState
        setPending p zt = zt { zoomPending = p }

zoomPutDouble :: ZoomTrackNo -> Int -> Double -> Zoom ()
zoomPutDouble trackNo t d = do
    zoomSetTime trackNo t
    zoomIncPending trackNo
    modifyTrack trackNo $ \z -> z
        { zoomBuilder = zoomBuilder z <> (fromWord64be . toWord64) d
        , zoomCount = (zoomCount z) + 1
        , zoomTrackData = updateZTSDouble (zoomCount z) d (zoomTrackData z)
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

zoomPutInt :: ZoomTrackNo -> Int -> Int -> Zoom ()
zoomPutInt trackNo t i = do
    zoomSetTime trackNo t
    zoomIncPending trackNo
    modifyTrack trackNo $ \z -> z
        { zoomBuilder = zoomBuilder z <> (fromInt32be . fromIntegral) i
        , zoomCount = (zoomCount z) + 1
        , zoomTrackData = updateZTSInt (zoomCount z) i (zoomTrackData z)
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

zoomFlush :: Zoom ()
zoomFlush = do
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
        flushTrack zt = (defTrackState (zoomType zt)) {zoomLevels = zoomLevels zt}

pushSummary :: Summary -> Zoom ()
pushSummary s = do
    deferSummary s
    zt'm <- IM.lookup (summaryTrack s) <$> gets zoomTracks
    maybe (return ()) pushSummary' zt'm
    where
        pushSummary' :: ZoomTrackState -> Zoom ()
        pushSummary' zt = do
            case IM.lookup (summaryLevel s) (zoomLevels zt) of
                Just (Just prev) -> do
                    let new = (prev `appendSummary` s) { summaryLevel = summaryLevel s + 1 }
                    insert Nothing
                    pushSummary new
                _                -> do
                    insert (Just s)
            where
                insert :: Maybe Summary -> Zoom ()
                insert x = modifyTrack (summaryTrack s) (\ztt ->
                    ztt { zoomLevels = IM.insert (summaryLevel s) x (zoomLevels ztt) } )

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

zoomBuildTrack :: ZoomTrackNo -> ZoomTrackState -> L.ByteString
zoomBuildTrack trackNo ZoomTrackState{..} =
    zoomPacketHeader <> no <> entryTime <> exitTime <> l <> bs
    where
        no = encInt trackNo
        entryTime = encInt zoomEntryTime
        exitTime = encInt zoomExitTime
        bs = toLazyByteString zoomBuilder
        l  = encInt . L.length $ bs

zoomBuildSummary :: ZoomTrackNo -> ZoomTrackState -> Summary
zoomBuildSummary trackNo ZoomTrackState{..} = build zoomType
    where
        build ZoomDouble = SummaryDouble
            { summaryTrack = trackNo
            , summaryLevel = 1
            , summaryEntryTime = zoomEntryTime
            , summaryExitTime = zoomExitTime
            , summaryDoubleEntry = ztsdEntry zoomTrackData
            , summaryDoubleExit = ztsdExit zoomTrackData
            , summaryDoubleMin = ztsdMin zoomTrackData
            , summaryDoubleMax = ztsdMax zoomTrackData
            , summaryAvg = ztsdSum zoomTrackData / (fromIntegral zoomCount)
            , summaryRMS = sqrt $ ztsSumSq  zoomTrackData / (fromIntegral zoomCount)
            }
        build ZoomInt = SummaryInt
            { summaryTrack = trackNo
            , summaryLevel = 1
            , summaryEntryTime = zoomEntryTime
            , summaryExitTime = zoomExitTime
            , summaryIntEntry = ztsiEntry zoomTrackData
            , summaryIntExit = ztsiExit zoomTrackData
            , summaryIntMin = ztsiMin zoomTrackData
            , summaryIntMax = ztsiMax zoomTrackData
            , summaryAvg = fromIntegral (ztsiSum zoomTrackData) / (fromIntegral zoomCount)
            , summaryRMS = sqrt $ ztsSumSq  zoomTrackData / (fromIntegral zoomCount)
            }

summaryToLazyByteString :: Summary -> L.ByteString
summaryToLazyByteString SummaryDouble{..} =
    zoomSummaryHeader <> no <> lvl <> entryTime <> exitTime <> l <> bs
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
    zoomSummaryHeader <> no <> lvl <> entryTime <> exitTime <> l <> bs
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


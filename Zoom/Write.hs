{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}

module Zoom.Write (
      Zoom
    , ZoomState(..)

    -- * State initialisation
    , zoomOpenW
    , zoomWithFileW

    -- * Data injection
    , zoomPutInt
    , zoomPutDouble
    
    , zoomFlush

    , zoomClose
) where

import Blaze.ByteString.Builder
import Control.Applicative ((<$>))
import Control.Monad.State
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Default
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Maybe (fromMaybe)
import Data.Monoid
import qualified Data.Foldable as Fold
import Data.Word
import System.IO
import Unsafe.Coerce (unsafeCoerce)

import Zoom.Common

------------------------------------------------------------

(<>) :: Monoid a => a -> a -> a
(<>) = mappend

data ZoomState = ZoomState
    { zoomHandle  :: Handle
    , zoomTracks  :: IntMap ZoomTrackState
    }

data ZoomTrackState = ZoomTrackState
    { zoomBuilder :: Builder
    , zoomPending :: Int
    , zoomTime    :: Maybe Int
    }

instance Default ZoomTrackState where
    def = ZoomTrackState mempty 1 Nothing

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
    L.hPut h zoomInitialHeader
    L.hPut h vMajor
    L.hPut h vMinor
    L.hPut h pNum
    L.hPut h pDen
    L.hPut h bNum
    L.hPut h bDen
    L.hPut h utc
    where
        buildInt16 :: Int -> L.ByteString
        buildInt16 = toLazyByteString . fromInt16le . fromIntegral
        buildInt64 :: Integer -> L.ByteString
        buildInt64 = toLazyByteString . fromInt64le . fromIntegral

zoomOpenW :: FilePath -> IO ZoomState
zoomOpenW path = do
    h <- openFile path WriteMode
    zoomWriteInitialHeader h
    return (ZoomState h IM.empty)

zoomWithFileW :: FilePath -> Zoom () -> IO ()
zoomWithFileW path f = do
    z <- zoomOpenW path
    z' <- execStateT f z
    zoomClose z'

modifyTracks :: (IntMap ZoomTrackState -> IntMap ZoomTrackState) -> Zoom ()
modifyTracks f = modify (\z -> z { zoomTracks = f (zoomTracks z) })

modifyTrack :: ZoomTrackNo -> (ZoomTrackState -> ZoomTrackState) -> Zoom ()
modifyTrack trackNo f = modifyTracks (IM.adjust f trackNo)

addTrack :: ZoomTrackNo -> Zoom ()
addTrack trackNo = modifyTracks (IM.insert trackNo def)

zoomSetTime :: ZoomTrackNo -> Int -> Zoom ()
zoomSetTime trackNo t = modifyTrack trackNo f
    where
        f zt = zt { zoomTime = Just $ maybe t id (zoomTime zt) }

zoomIncPending :: ZoomTrackNo -> Zoom ()
zoomIncPending trackNo = do
    zt <- IM.lookup trackNo <$> gets zoomTracks
    case zt of
        Just track -> do
            let p = zoomPending track
            if (p >= 1024)
                then do
                    z <- get
                    z' <- liftIO $ zoomFlush z
                    put z'
                    modifyTrack trackNo (setPending 1)
                else
                    modifyTrack trackNo (setPending (p+1))
        Nothing -> addTrack trackNo
    where
        setPending :: Int -> ZoomTrackState -> ZoomTrackState
        setPending p zt = zt { zoomPending = p }

zoomPutInt :: ZoomTrackNo -> Int -> Int -> Zoom ()
zoomPutInt trackNo t d = do
    zoomSetTime trackNo t
    zoomIncPending trackNo
    modifyTrack trackNo $ \z -> z { zoomBuilder = zoomBuilder z <> (fromInt32le . fromIntegral) d }

zoomPutDouble :: ZoomTrackNo -> Int -> Double -> Zoom ()
zoomPutDouble trackNo t d = do
    zoomSetTime trackNo t
    zoomIncPending trackNo
    modifyTrack trackNo $ \z -> z { zoomBuilder = zoomBuilder z <> (fromWord64be . toWord64) d }

zoomFlush :: ZoomState -> IO ZoomState
zoomFlush z@ZoomState{..} = do
    let b = IM.mapWithKey zoomBuildTrack zoomTracks
    Fold.mapM_ (L.hPut zoomHandle) b
    return z { zoomTracks = IM.map flushTrack zoomTracks }
    where
        flushTrack :: ZoomTrackState -> ZoomTrackState
        flushTrack _ = def

zoomBuildTrack :: ZoomTrackNo -> ZoomTrackState -> L.ByteString
zoomBuildTrack trackNo ZoomTrackState{..} =
    zoomPacketHeader <> no <> t' <> l <> bs
    where
         no = toLazyByteString . fromInt32le . fromIntegral $ trackNo
         bs = toLazyByteString zoomBuilder
         l  = toLazyByteString . fromInt32le . fromIntegral . L.length $ bs
         t' = toLazyByteString . fromInt32le . fromIntegral $ fromMaybe 0 zoomTime
    
zoomClose :: ZoomState -> IO ()
zoomClose z@ZoomState{..} = do
    _ <- zoomFlush z
    hClose zoomHandle

toWord64 :: Double -> Word64
toWord64 = unsafeCoerce


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
import Control.Monad.State
import qualified Data.ByteString.Lazy as L
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid
import Data.Word
import System.IO
import Unsafe.Coerce (unsafeCoerce)

import Zoom.Common

------------------------------------------------------------

(<>) :: Builder -> Builder -> Builder
(<>) = mappend

data ZoomState = ZoomState
    { zoomHandle  :: Handle
    , zoomBuilder :: Builder
    , zoomPending :: Int
    , zoomTime    :: Maybe Int
    }

type Zoom = StateT ZoomState IO

zoomOpenW :: FilePath -> IO ZoomState
zoomOpenW path = do
    h <- openFile path WriteMode
    return (ZoomState h mempty 0 Nothing)

zoomWithFileW :: FilePath -> Zoom () -> IO ()
zoomWithFileW path f = do
    z <- zoomOpenW path
    z' <- execStateT f z
    zoomClose z'

zoomSetTime :: Int -> Zoom ()
zoomSetTime t = do
    t' <- gets zoomTime
    when (isNothing t') . modify $ \z -> z { zoomTime = Just t }

zoomIncPending :: Zoom ()
zoomIncPending = do
    p <- gets zoomPending
    if (p >= 1024) 
        then do
            z <- get
            z' <- liftIO $ zoomFlush z
            put $ z' { zoomPending = 1 }
        else
            modify $ \z -> z { zoomPending = p+1 }

zoomPutInt :: Int -> Int -> Zoom ()
zoomPutInt t d = do
    zoomSetTime t
    zoomIncPending
    modify $ \z -> z { zoomBuilder = zoomBuilder z <> (fromInt32le . fromIntegral) d }

zoomPutDouble :: Int -> Double -> Zoom ()
zoomPutDouble t d = do
    zoomSetTime t
    zoomIncPending
    modify $ \z -> z { zoomBuilder = zoomBuilder z <> (fromWord64be . toWord64) d }

zoomFlush :: ZoomState -> IO ZoomState
zoomFlush z@ZoomState{..} = do
    let bs = toLazyByteString zoomBuilder
        l  = toLazyByteString . fromInt32le . fromIntegral . L.length $ bs
        t' = toLazyByteString . fromInt32le . fromIntegral $ fromMaybe 0 zoomTime
    L.hPut zoomHandle zoomHeader
    L.hPut zoomHandle t'
    L.hPut zoomHandle l
    L.hPut zoomHandle bs
    return z { zoomBuilder = mempty
             , zoomTime = Nothing
             }
    
zoomClose :: ZoomState -> IO ()
zoomClose z@ZoomState{..} = do
    _ <- zoomFlush z
    hClose zoomHandle

toWord64 :: Double -> Word64
toWord64 = unsafeCoerce


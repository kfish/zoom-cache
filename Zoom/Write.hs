{-# LANGUAGE RecordWildCards #-}

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
import Control.Monad (replicateM_)
import Control.Monad.State
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Bits
import qualified Data.ByteString as BS
import Data.Default
import Data.Iteratee (Iteratee)
import qualified Data.Iteratee as I
import Data.Monoid
import Data.Word
import System.IO
import UI.Command
import Unsafe.Coerce (unsafeCoerce)

------------------------------------------------------------

(<>) = mappend

data ZoomState = ZoomState
    { zoomHandle  :: Handle
    , zoomBuilder :: Builder
    }

type Zoom = StateT ZoomState IO

zoomOpenW :: FilePath -> IO ZoomState
zoomOpenW path = do
    h <- openFile path WriteMode
    return (ZoomState h mempty)

zoomWithFileW :: FilePath -> Zoom () -> IO ()
zoomWithFileW path f = do
    z <- zoomOpenW path
    z' <- execStateT f z
    zoomClose z'

zoomPutInt :: Int -> Zoom ()
zoomPutInt d = modify $ \z -> z { zoomBuilder = zoomBuilder z <> (fromInt32le . fromIntegral) d }

zoomPutDouble :: Double -> Zoom ()
zoomPutDouble d = modify $ \z -> z { zoomBuilder = zoomBuilder z <> (fromWord64be . toWord64) d }

zoomFlush :: ZoomState -> IO ZoomState
zoomFlush z@ZoomState{..} = do
    toByteStringIO (BS.hPut zoomHandle) zoomBuilder
    return z { zoomBuilder = mempty }
    
zoomClose :: ZoomState -> IO ()
zoomClose z@ZoomState{..} = do
    _ <- zoomFlush z
    hClose zoomHandle

toWord64 :: Double -> Word64
toWord64 = unsafeCoerce


{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}

module Zoom.Read (
      zoomDumpFile
    , zoomReadFile
) where

import Control.Applicative ((<$>))
import Control.Monad (forever, replicateM, when)
import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.Trans (lift, MonadIO)
import qualified Data.ByteString.Lazy as L
import Data.Iteratee (Iteratee)
import qualified Data.Iteratee as I
import Data.Word
import Unsafe.Coerce (unsafeCoerce)

import Zoom.Common

------------------------------------------------------------

data Packet = Packet
    { packetTimeStamp :: Int
    , packetLength :: Int
    , packetData :: [Double]
    }

zoomDumpFile :: [FilePath] -> IO ()
zoomDumpFile = zoomReadFile dumpData

zoomReadFile :: (Functor m, MonadCatchIO m) => (Packet -> m ()) -> [FilePath] -> m ()
zoomReadFile _ []       = return ()
zoomReadFile f (path:_) = I.fileDriverRandom (zReader f) path

zReader :: (Functor m, MonadIO m) => (Packet -> m ()) -> Iteratee [Word8] m ()
zReader f = forever (zReadPacket f)

dumpTime :: Packet -> IO ()
dumpTime Packet{..} = print packetTimeStamp

dumpData :: Packet -> IO ()
dumpData Packet{..} = mapM_ print packetData

zReadPacket :: (Functor m, MonadIO m) =>
               (Packet -> m ()) -> Iteratee [Word8] m ()
zReadPacket f = do
    h <- I.joinI $ I.takeUpTo 4 I.stream2list -- header
    when (h == L.unpack zoomHeader) $ do
        t <- zReadInt32 -- timestamp
        n <- flip div 8 <$> zReadInt32
        d <- replicateM n zReadFloat64be
        lift $ f (Packet t n d)

zReadInt32 :: (Functor m, MonadIO m) => Iteratee [Word8] m Int
zReadInt32 = fromIntegral <$> I.endianRead4 I.LSB

zReadFloat64be :: (Functor m, MonadIO m) => Iteratee [Word8] m Double
zReadFloat64be = do
    n <- I.endianRead8 I.MSB
    return (unsafeCoerce n :: Double)


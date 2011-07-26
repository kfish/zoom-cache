{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}

module Zoom.Read (
      zoomDumpFile
    , zoomReadFile
) where

import Control.Applicative ((<$>))
import Control.Monad (forever, replicateM_, when)
import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.Trans (lift, MonadIO)
import Data.Bits
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
zoomDumpFile = zoomReadFile dumpTime

zoomReadFile :: (Functor m, MonadCatchIO m) => (Packet -> m ()) -> [FilePath] -> m ()
zoomReadFile _ []       = return ()
zoomReadFile f (path:_) = I.fileDriverRandom (zReader f) path

zReader :: (Functor m, MonadIO m) => (Packet -> m ()) -> Iteratee [Word8] m ()
zReader f = forever (zReadPacket f)

dumpTime :: Packet -> IO ()
dumpTime Packet{..} = print packetTimeStamp

zReadPacket :: (Functor m, MonadIO m) =>
               (Packet -> m ()) -> Iteratee [Word8] m ()
zReadPacket f = do
    h <- I.joinI $ I.takeUpTo 4 I.stream2list -- header
    when (h == L.unpack zoomHeader) $ do
        t <- zReadInt32 -- timestamp
        n <- flip div 8 <$> zReadInt32
        -- replicateM_ n (zReadFloat64be >>= liftIO . putStrLn . show)
        replicateM_ n zReadFloat64be
        lift $ f (Packet t n [])

zReadInt32 :: (Functor m, MonadIO m) => Iteratee [Word8] m Int
zReadInt32 = fromIntegral <$> I.endianRead4 I.LSB

zEndianRead8 :: (Functor m, MonadIO m) => I.Endian -> Iteratee [Word8] m Word64
zEndianRead8 e = do
    c1 <- I.head
    c2 <- I.head
    c3 <- I.head
    c4 <- I.head
    c5 <- I.head
    c6 <- I.head
    c7 <- I.head
    c8 <- I.head
    case e of
        I.MSB -> return $ (((((((((((((fromIntegral c1
                          `shiftL` 8) .|. fromIntegral c2)
                          `shiftL` 8) .|. fromIntegral c3)
                          `shiftL` 8) .|. fromIntegral c4)
                          `shiftL` 8) .|. fromIntegral c5)
                          `shiftL` 8) .|. fromIntegral c6)
                          `shiftL` 8) .|. fromIntegral c7)
                          `shiftL` 8) .|. fromIntegral c8
        I.LSB -> return $ (((((((((((((fromIntegral c8
                          `shiftL` 8) .|. fromIntegral c7)
                          `shiftL` 8) .|. fromIntegral c6)
                          `shiftL` 8) .|. fromIntegral c5)
                          `shiftL` 8) .|. fromIntegral c4)
                          `shiftL` 8) .|. fromIntegral c3)
                          `shiftL` 8) .|. fromIntegral c2)
                          `shiftL` 8) .|. fromIntegral c1

zReadFloat64be :: (Functor m, MonadIO m) => Iteratee [Word8] m Double
zReadFloat64be = do
    n <- zEndianRead8 I.MSB
    return (unsafeCoerce n :: Double)


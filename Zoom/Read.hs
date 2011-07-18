{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}

module Zoom.Read (
    zoomReadFile
) where

import Control.Applicative ((<$>))
import Control.Monad (forever, replicateM_)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Bits
import Data.Iteratee (Iteratee)
import qualified Data.Iteratee as I
import Data.Word
import Unsafe.Coerce (unsafeCoerce)

------------------------------------------------------------

zoomReadFile :: [FilePath] -> IO ()
zoomReadFile []       = return ()
zoomReadFile (path:_) = I.fileDriverRandom zReader path

zReader :: (Functor m, MonadIO m) => Iteratee [Word8] m ()
zReader = forever zReadPacket

zReadPacket :: (Functor m, MonadIO m) => Iteratee [Word8] m ()
zReadPacket = do
    I.drop 4
    n <- flip div 8 <$> zReadInt32
    replicateM_ n (zReadFloat64be >>= liftIO . putStrLn . show)

zReadInt32 :: (Functor m, MonadIO m) => Iteratee [Word8] m Int
zReadInt32 = fromIntegral <$> I.endianRead4 I.LSB

zReadFloat64be :: (Functor m, MonadIO m) => Iteratee [Word8] m Double
zReadFloat64be = do
    c1 <- I.head
    c2 <- I.head
    c3 <- I.head
    c4 <- I.head
    c5 <- I.head
    c6 <- I.head
    c7 <- I.head
    c8 <- I.head
    let n :: Word64
        n = (((((((((((((fromIntegral c1
             `shiftL` 8) .|. fromIntegral c2)
             `shiftL` 8) .|. fromIntegral c3)
             `shiftL` 8) .|. fromIntegral c4)
             `shiftL` 8) .|. fromIntegral c5)
             `shiftL` 8) .|. fromIntegral c6)
             `shiftL` 8) .|. fromIntegral c7)
             `shiftL` 8) .|. fromIntegral c8
    return (unsafeCoerce n :: Double)


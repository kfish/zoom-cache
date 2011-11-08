{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      : Data.Iteratee.ZoomCache.Utils
-- Copyright   : Conrad Parker
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Conrad Parker <conrad@metadecks.org>
-- Stability   : unstable
-- Portability : unknown
--
-- Iteratee reading of ZoomCache files.
----------------------------------------------------------------------

module Data.Iteratee.ZoomCache.Utils (
    -- * Raw data reading iteratees
      readInt8
    , readInt16be
    , readInt32be
    , readInt64be
    , readFloat32be
    , readDouble64be
    , readRational64be
) where

import Control.Applicative ((<$>))
import Control.Monad.Trans (MonadIO)
import qualified Data.ByteString as B
import Data.Int
import Data.Iteratee (Iteratee)
import qualified Data.Iteratee as I
import qualified Data.ListLike as LL
import Data.Ratio
import Data.Word
import Unsafe.Coerce (unsafeCoerce)

-- | Read 1 byte as an Integral
readInt8 :: (I.Nullable s, LL.ListLike s Word8, Functor m, MonadIO m, Integral a)
         => Iteratee s m a
readInt8 = fromIntegral . u8_to_s8 <$> I.head
    where
        u8_to_s8 :: Word8 -> Int8
        u8_to_s8 = fromIntegral
{-# SPECIALIZE INLINE readInt8 :: (Functor m, MonadIO m) => Iteratee [Word8] m Int8 #-}
{-# SPECIALIZE INLINE readInt8 :: (Functor m, MonadIO m) => Iteratee B.ByteString m Int8 #-}
{-# SPECIALIZE INLINE readInt8 :: (Functor m, MonadIO m) => Iteratee [Word8] m Int #-}
{-# SPECIALIZE INLINE readInt8 :: (Functor m, MonadIO m) => Iteratee B.ByteString m Int #-}

-- | Read 2 bytes as a big-endian Integral
readInt16be :: (I.Nullable s, LL.ListLike s Word8, Functor m, MonadIO m, Integral a)
            => Iteratee s m a
readInt16be = fromIntegral . u16_to_s16 <$> I.endianRead2 I.MSB
    where
        u16_to_s16 :: Word16 -> Int16
        u16_to_s16 = fromIntegral
{-# SPECIALIZE INLINE readInt16be :: (Functor m, MonadIO m) => Iteratee [Word8] m Int16 #-}
{-# SPECIALIZE INLINE readInt16be :: (Functor m, MonadIO m) => Iteratee B.ByteString m Int16 #-}
{-# SPECIALIZE INLINE readInt16be :: (Functor m, MonadIO m) => Iteratee [Word8] m Int #-}
{-# SPECIALIZE INLINE readInt16be :: (Functor m, MonadIO m) => Iteratee B.ByteString m Int #-}

-- | Read 4 bytes as a big-endian Integral
readInt32be :: (I.Nullable s, LL.ListLike s Word8, Functor m, MonadIO m, Integral a)
            => Iteratee s m a
readInt32be = fromIntegral . u32_to_s32 <$> I.endianRead4 I.MSB
    where
        u32_to_s32 :: Word32 -> Int32
        u32_to_s32 = fromIntegral
{-# SPECIALIZE INLINE readInt32be :: (Functor m, MonadIO m) => Iteratee [Word8] m Int32 #-}
{-# SPECIALIZE INLINE readInt32be :: (Functor m, MonadIO m) => Iteratee B.ByteString m Int32 #-}
{-# SPECIALIZE INLINE readInt32be :: (Functor m, MonadIO m) => Iteratee [Word8] m Int #-}
{-# SPECIALIZE INLINE readInt32be :: (Functor m, MonadIO m) => Iteratee B.ByteString m Int #-}

-- | Read 8 bytes as a big-endian Integral
readInt64be :: (I.Nullable s, LL.ListLike s Word8, Functor m, MonadIO m, Integral a)
            => Iteratee s m a
readInt64be = fromIntegral . u64_to_s64 <$> I.endianRead8 I.MSB
    where
        u64_to_s64 :: Word64 -> Int64
        u64_to_s64 = fromIntegral
{-# SPECIALIZE INLINE readInt64be :: (Functor m, MonadIO m) => Iteratee [Word8] m Int64 #-}
{-# SPECIALIZE INLINE readInt64be :: (Functor m, MonadIO m) => Iteratee B.ByteString m Int64 #-}
{-# SPECIALIZE INLINE readInt64be :: (Functor m, MonadIO m) => Iteratee [Word8] m Int #-}
{-# SPECIALIZE INLINE readInt64be :: (Functor m, MonadIO m) => Iteratee B.ByteString m Int #-}

-- | Read 4 bytes as a big-endian Float
readFloat32be :: (I.Nullable s, LL.ListLike s Word8, Functor m, MonadIO m)
               => Iteratee s m Float
readFloat32be = do
    n <- I.endianRead4 I.MSB
    return (unsafeCoerce n :: Float)
{-# SPECIALIZE INLINE readFloat32be :: (Functor m, MonadIO m) => Iteratee [Word8] m Float #-}
{-# SPECIALIZE INLINE readFloat32be :: (Functor m, MonadIO m) => Iteratee B.ByteString m Float #-}

-- | Read 8 bytes as a big-endian Double
readDouble64be :: (I.Nullable s, LL.ListLike s Word8, Functor m, MonadIO m)
               => Iteratee s m Double
readDouble64be = do
    n <- I.endianRead8 I.MSB
    return (unsafeCoerce n :: Double)
{-# SPECIALIZE INLINE readDouble64be :: (Functor m, MonadIO m) => Iteratee [Word8] m Double #-}
{-# SPECIALIZE INLINE readDouble64be :: (Functor m, MonadIO m) => Iteratee B.ByteString m Double #-}

-- | Read 16 bytes as a big-endian Rational, encoded as an 8 byte
-- big endian numerator followed by an 8 byte big endian denominator.
readRational64be :: (I.Nullable s, LL.ListLike s Word8, Functor m, MonadIO m)
                 => Iteratee s m Rational
readRational64be = do
    (num :: Integer) <- readInt64be
    (den :: Integer) <- readInt64be
    if (den == 0)
        then return 0
        else return (num % den)



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
      readInt16be
    , readInt32be
    , readInt64be
    , readDouble64be
    , readRational64be
) where

import Control.Applicative ((<$>))
import Control.Monad.Trans (MonadIO)
import Data.Int
import Data.Iteratee (Iteratee)
import qualified Data.Iteratee as I
import Data.Ratio
import Data.Word
import Unsafe.Coerce (unsafeCoerce)

-- | Read 2 bytes as a big-endian Int.
readInt16be :: (Functor m, MonadIO m) => Iteratee [Word8] m Int
readInt16be = fromIntegral . u16_to_s16 <$> I.endianRead2 I.MSB
    where
        u16_to_s16 :: Word16 -> Int16
        u16_to_s16 = fromIntegral

-- | Read 4 bytes as a big-endian Int.
readInt32be :: (Functor m, MonadIO m) => Iteratee [Word8] m Int
readInt32be = fromIntegral . u32_to_s32 <$> I.endianRead4 I.MSB
    where
        u32_to_s32 :: Word32 -> Int32
        u32_to_s32 = fromIntegral

-- | Read 8 bytes as a big-endian Integer
readInt64be :: (Functor m, MonadIO m, Integral a) => Iteratee [Word8] m a
readInt64be = fromIntegral . u64_to_s64 <$> I.endianRead8 I.MSB
    where
        u64_to_s64 :: Word64 -> Int64
        u64_to_s64 = fromIntegral
{-# SPECIALIZE INLINE readInt64be :: (Functor m, MonadIO m) => Iteratee [Word8] m Int #-}
{-# SPECIALIZE INLINE readInt64be :: (Functor m, MonadIO m) => Iteratee [Word8] m Int64 #-}

-- | Read 8 bytes as a big-endian Double
readDouble64be :: (Functor m, MonadIO m) => Iteratee [Word8] m Double
readDouble64be = do
    n <- I.endianRead8 I.MSB
    return (unsafeCoerce n :: Double)

-- | Read 16 bytes as a big-endian Rational, encoded as an 8 byte
-- big endian numerator followed by an 8 byte big endian denominator.
readRational64be :: (Functor m, MonadIO m) => Iteratee [Word8] m Rational
readRational64be = do
    (num :: Integer) <- readInt64be
    (den :: Integer) <- readInt64be
    if (den == 0)
        then return 0
        else return (num % den)



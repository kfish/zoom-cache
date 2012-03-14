{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      : Blaze.ByteString.Builder.ZoomCache
-- Copyright   : Conrad Parker
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Conrad Parker <conrad@metadecks.org>
-- Stability   : unstable
-- Portability : unknown
--
-- Blaze-builder utility functions for writing ZoomCache files.
----------------------------------------------------------------------

module Blaze.ByteString.Builder.ZoomCache (
    -- * Creating builders for ZoomCache types
      fromSampleOffset

    -- * Creating builders from numeric types used by ZoomCache
    , fromFloat
    , fromDouble
    , fromIntegral32be
    , fromIntegerVLC
    , fromRational64
) where

import Blaze.ByteString.Builder
import Data.Bits
import Data.Monoid
import Data.Ratio
import Data.Word
import Unsafe.Coerce (unsafeCoerce)

import Data.ZoomCache.Common

----------------------------------------------------------------------
-- Creating builders for ZoomCache types.

-- | Serialize a 'TimeStamp' in 64bit big endian format.
fromSampleOffset :: SampleOffset -> Builder
fromSampleOffset = fromInt64be . fromIntegral . unSO
{-# INLINE fromSampleOffset #-}

----------------------------------------------------------------------
-- Creating builders from numeric types used by ZoomCache.

-- | Serialize a 'Float' in big-endian IEEE 754-2008 binary32 format
-- (IEEE 754-1985 single format).
fromFloat :: Float -> Builder
fromFloat = fromWord32be . toWord32
    where
        toWord32 :: Float -> Word32
        toWord32 = unsafeCoerce
{-# INLINE fromFloat #-}

-- | Serialize a 'Double' in big-endian IEEE 754-2008 binary64 format
-- (IEEE 754-1985 double format).
fromDouble :: Double -> Builder
fromDouble = fromWord64be . toWord64
    where
        toWord64 :: Double -> Word64
        toWord64 = unsafeCoerce
{-# INLINE fromDouble #-}

-- | Serialize an 'Integral' in 32bit big endian format.
fromIntegral32be :: forall a . (Integral a) => a -> Builder
fromIntegral32be = fromInt32be . fromIntegral
{-# SPECIALIZE INLINE fromIntegral32be :: Int -> Builder #-}
{-# SPECIALIZE INLINE fromIntegral32be :: Integer -> Builder #-}

-- | Serialize an 'Integer' in variable-length-coding format
-- For details of the variable-length coding format, see
-- "Data.ZoomCache.Numeric.Int".
fromIntegerVLC :: Integer -> Builder
fromIntegerVLC x0 = enc x1 `mappend` buildVLC xHi
    where
        x1 = (xLo `shiftL` 1) .|. sign0
        sign0 | x0 < 0    = 1
              | otherwise = 0

        (xHi, xLo) = bCont 6 (abs x0)

        -- Split a bitstring of length len into a tuple of
        -- the top (len-n) bits and (the lower n bits with the
        -- extension bit set if any of the top (len-n) bits are
        -- non-zero). We assume n < 8.
        bCont :: (Num a, Bits a) => Int -> a -> (a, a)
        bCont n v 
            | hi == 0   = (hi, lo)
            | otherwise = (hi, lo .|. (2^n))
            where
                -- Split a bitstring of length len into a tuple of
                -- the top (len-n) bits and the lower n bits.
                (hi, lo) = (v `shiftR` n, v .&. (2^n -1))

        -- Build a variable-length-coded sequence of bytes corresponding
        -- to the contents of x, 7 bits at a time.
        buildVLC x
            | x  == 0   = mempty
            | otherwise = enc lo `mappend` buildVLC hi
            where
                (hi, lo) = bCont 7 x

        enc :: Integer -> Builder
        enc = fromWord8 . fromIntegral

-- | Serialize a 'Rational' as a sequence of two 64bit big endian format
-- integers.
fromRational64 :: Rational -> Builder
fromRational64 r = mconcat
    [ fromInt64be . fromIntegral . numerator $ r
    , fromInt64be . fromIntegral . denominator $ r
    ]
{-# INLINE fromRational64 #-}

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
      fromTimeStamp

    -- * Creating builders from numeric types used by ZoomCache
    , fromFloat
    , fromDouble
    , fromIntegral32be
    , fromRational64
) where

import Blaze.ByteString.Builder
import Data.Monoid
import Data.Ratio
import Data.Word
import Unsafe.Coerce (unsafeCoerce)

import Data.ZoomCache.Common

----------------------------------------------------------------------
-- Creating builders for ZoomCache types.

-- | Serialize a 'TimeStamp' in 64bit big endian format.
fromTimeStamp :: TimeStamp -> Builder
fromTimeStamp = fromInt64be . fromIntegral . unTS
{-# INLINE fromTimeStamp #-}

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

-- | Serialize a 'Rational' as a sequence of two 64bit big endian format
-- integers.
fromRational64 :: Rational -> Builder
fromRational64 r = mconcat
    [ fromInt64be . fromIntegral . numerator $ r
    , fromInt64be . fromIntegral . denominator $ r
    ]
{-# INLINE fromRational64 #-}

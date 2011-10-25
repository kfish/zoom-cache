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
    -- * Raw data iteratees
      zReadInt16
    , zReadInt32
    , zReadInt64
    , zReadFloat64be
    , readRational64
) where

import Control.Applicative ((<$>))
import Control.Monad.Trans (MonadIO)
import Data.Int
import Data.Iteratee (Iteratee)
import qualified Data.Iteratee as I
import Data.Ratio
import Data.Word
import Unsafe.Coerce (unsafeCoerce)

zReadInt16 :: (Functor m, MonadIO m) => Iteratee [Word8] m Int
zReadInt16 = fromIntegral . u16_to_s16 <$> I.endianRead2 I.MSB
    where
        u16_to_s16 :: Word16 -> Int16
        u16_to_s16 = fromIntegral

zReadInt32 :: (Functor m, MonadIO m) => Iteratee [Word8] m Int
zReadInt32 = fromIntegral . u32_to_s32 <$> I.endianRead4 I.MSB
    where
        u32_to_s32 :: Word32 -> Int32
        u32_to_s32 = fromIntegral

zReadInt64 :: (Functor m, MonadIO m) => Iteratee [Word8] m Integer
zReadInt64 = fromIntegral . u64_to_s64 <$> I.endianRead8 I.MSB
    where
        u64_to_s64 :: Word64 -> Int64
        u64_to_s64 = fromIntegral

zReadFloat64be :: (Functor m, MonadIO m) => Iteratee [Word8] m Double
zReadFloat64be = do
    n <- I.endianRead8 I.MSB
    return (unsafeCoerce n :: Double)

readRational64 :: (Functor m, MonadIO m) => Iteratee [Word8] m Rational
readRational64 = do
    num <- zReadInt64
    den <- zReadInt64
    if (den == 0)
        then return 0
        else return $ (fromIntegral num) % (fromIntegral den)



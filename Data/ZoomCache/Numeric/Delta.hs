{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      : Data.ZoomCache.Numeric.Delta
-- Copyright   : Conrad Parker
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Conrad Parker <conrad@metadecks.org>
-- Stability   : unstable
-- Portability : unknown
--
-- Delta encoding and decoding of numeric values.
----------------------------------------------------------------------

module Data.ZoomCache.Numeric.Delta (
  -- * Delta encoding
    deltaEncode
  , deltaDecode
) where

-- | Delta encode a list of numbers
--
-- > > deltaEncode [1,2,3,2,4]
-- > [1,1,1,-1,2]
deltaEncode :: Num a => [a] -> [a]
deltaEncode xs = zipWith (-) xs (0:xs)
{-# INLINE deltaEncode #-}

-- | Delta-decode a list of numbers
--
-- > > deltaDecode [1,1,1,-1,2]
-- > [1,2,3,2,4]
deltaDecode :: Num a => [a] -> [a]
deltaDecode = scanl1 (+)
{-# INLINE deltaDecode #-}

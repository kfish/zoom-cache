{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
{- |
   Module      : Data.ZoomCache.Multichannel.List
   Copyright   : Conrad Parker
   License     : BSD3-style (see LICENSE)

   Maintainer  : Conrad Parker <conrad@metadecks.org>
   Stability   : unstable
   Portability : unknown

Default codec implementation for multichannel values of type [a].

This module implements the interfaces documented in "Data.ZoomCache.Codec".

Multichannel SummaryData is simply a concatenation of n blocks of SummaryData
for type a.

-}
----------------------------------------------------------------------

module Data.ZoomCache.Multichannel.List (
      SummaryData(..)
    , SummaryWork(..)
)where

import Data.TypeLevel.Num hiding ((==))

import Data.ZoomCache.Codec
import Data.ZoomCache.Multichannel.NList
import Data.ZoomCache.NList

----------------------------------------------------------------------

instance (ZoomWrite a, ZoomWritable a) => ZoomWrite [a] where
    write = writeList

writeList :: (ZoomWrite a, ZoomWritable a) => TrackNo -> [a] -> ZoomW ()
writeList tn xs = reifyIntegral (length xs) (\n -> write tn (NList n xs))

instance (ZoomWrite a, ZoomWritable a) => ZoomWrite (SampleOffset, [a]) where
    write = writeSOList

writeSOList :: (ZoomWrite a, ZoomWritable a) => TrackNo -> (SampleOffset, [a]) -> ZoomW ()
writeSOList tn (ts, xs) = reifyIntegral (length xs) (\n -> write tn (ts, (NList n xs)))

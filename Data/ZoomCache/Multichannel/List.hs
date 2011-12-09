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

    , wholeTrackSummaryListDouble
    , enumSummaryListDouble
)where

import Control.Applicative ((<$>))
import Control.Monad.Trans (MonadIO)
import Data.ByteString (ByteString)
import Data.Int
import qualified Data.Iteratee as I
import Data.Maybe
import Data.Typeable
import Data.TypeLevel.Num hiding ((==))
import Data.Word

import Data.Iteratee.ZoomCache
import Data.ZoomCache.Codec
import Data.ZoomCache.Multichannel.NList
import Data.ZoomCache.NList
import Data.ZoomCache.Numeric

----------------------------------------------------------------------

instance (ZoomWrite a, ZoomWritable a) => ZoomWrite [a] where
    write = writeList

writeList :: (ZoomWrite a, ZoomWritable a) => TrackNo -> [a] -> ZoomW ()
writeList tn xs = reifyIntegral (length xs) (\n -> write tn (NList n xs))

instance (ZoomWrite a, ZoomWritable a) => ZoomWrite (SampleOffset, [a]) where
    write = writeSOList

writeSOList :: (ZoomWrite a, ZoomWritable a) => TrackNo -> (SampleOffset, [a]) -> ZoomW ()
writeSOList tn (ts, xs) = reifyIntegral (length xs) (\n -> write tn (ts, (NList n xs)))

----------------------------------------------------------------------

toSummaryListDouble :: Typeable a => Summary a -> Maybe [Summary Double]
toSummaryListDouble s | typeOf s == typeOf (undefined :: Summary (NList D1 Double)) =
                                sl <$> (cast s :: Maybe (Summary (NList D1 Double)))
                      | typeOf s == typeOf (undefined :: Summary (NList D1 Float)) =
                               sld <$> (cast s :: Maybe (Summary (NList D1 Float)))
                      | typeOf s == typeOf (undefined :: Summary (NList D1 Int)) =
                               sld <$> (cast s :: Maybe (Summary (NList D1 Int)))
                      | typeOf s == typeOf (undefined :: Summary (NList D1 Int8)) =
                               sld <$> (cast s :: Maybe (Summary (NList D1 Int8)))
                      | typeOf s == typeOf (undefined :: Summary (NList D1 Int16)) =
                               sld <$> (cast s :: Maybe (Summary (NList D1 Int16)))
                      | typeOf s == typeOf (undefined :: Summary (NList D1 Int32)) =
                               sld <$> (cast s :: Maybe (Summary (NList D1 Int32)))
                      | typeOf s == typeOf (undefined :: Summary (NList D1 Int64)) =
                               sld <$> (cast s :: Maybe (Summary (NList D1 Int64)))
                      | typeOf s == typeOf (undefined :: Summary (NList D1 Integer)) =
                               sld <$> (cast s :: Maybe (Summary (NList D1 Integer)))
                      | typeOf s == typeOf (undefined :: Summary (NList D1 Word)) =
                               sld <$> (cast s :: Maybe (Summary (NList D1 Word)))
                      | typeOf s == typeOf (undefined :: Summary (NList D1 Word8)) =
                               sld <$> (cast s :: Maybe (Summary (NList D1 Word8)))
                      | typeOf s == typeOf (undefined :: Summary (NList D1 Word16)) =
                               sld <$> (cast s :: Maybe (Summary (NList D1 Word16)))
                      | typeOf s == typeOf (undefined :: Summary (NList D1 Word32)) =
                               sld <$> (cast s :: Maybe (Summary (NList D1 Word32)))
                      | typeOf s == typeOf (undefined :: Summary (NList D1 Word64)) =
                               sld <$> (cast s :: Maybe (Summary (NList D1 Word64)))
                      | otherwise = Nothing
    where
        sl :: Summary (NList D1 a) -> [Summary a]
        sl = summaryNListToList
        sld :: Typeable a => Summary (NList D1 a) -> [Summary Double]
        sld = catMaybes . map toSummaryDouble . sl

----------------------------------------------------------------------

-- | Read the summary of an entire track.
wholeTrackSummaryListDouble :: (Functor m, MonadIO m)
                            => [IdentifyCodec]
                            -> TrackNo
                            -> I.Iteratee ByteString m [Summary Double]
wholeTrackSummaryListDouble identifiers trackNo =
    I.joinI $ enumCacheFile identifiers .
    I.joinI . filterTracks [trackNo] .  I.joinI . e $ I.last
    where
        e = I.joinI . enumSummaries . I.mapChunks (catMaybes . map toSLD)
        toSLD :: ZoomSummary -> Maybe [Summary Double]
        toSLD (ZoomSummary s) = toSummaryListDouble s

enumSummaryListDouble :: (Functor m, MonadIO m)
                      => Int
                      -> I.Enumeratee [Stream] [[Summary Double]] m a
enumSummaryListDouble level =
    I.joinI . enumSummaryLevel level .
    I.mapChunks (catMaybes . map toSLD)
    where
        toSLD :: ZoomSummary -> Maybe [Summary Double]
        toSLD (ZoomSummary s) = toSummaryListDouble s

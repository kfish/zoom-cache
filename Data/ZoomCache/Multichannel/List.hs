{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
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
    , supportMultichannel
    , identifyCodecMultichannel
    , oneTrackMultichannel
    , mkTrackSpecMultichannel
)where

import Control.Applicative ((<$>))
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Functor.Identity
import qualified Data.IntMap as IM
import qualified Data.Iteratee as I
import Data.TypeLevel.Num hiding ((==))

import Data.ZoomCache.Codec
import Data.ZoomCache.Common
import Data.ZoomCache.Multichannel.Common
import Data.ZoomCache.Multichannel.NList
import Data.ZoomCache.NList
import Data.ZoomCache.Types
import Data.Iteratee.ZoomCache.Utils

----------------------------------------------------------------------

supportMultichannel :: [IdentifyCodec] -> [IdentifyCodec]
supportMultichannel = f
    where f x = x ++ [identifyCodecMultichannel (f x)]

runner1 :: Identity (I.Iteratee s Identity c) -> c
runner1 = runIdentity . I.run . runIdentity

identifyCodecMultichannel :: [IdentifyCodec] -> IdentifyCodec
identifyCodecMultichannel identifiers bs = runner1 $ I.enumPure1Chunk bs identifyMulti
    where
        identifyMulti :: (Functor m, Monad m) => I.Iteratee ByteString m (Maybe Codec)
        identifyMulti = do
            mIdent <- B.pack <$> (I.joinI $ I.takeUpTo 8 I.stream2list)
            if mIdent == trackTypeMultichannel
                then do
                    channels <- readInt32be
                    subIdentLength <- readInt32be
                    subCodec <- readCodec identifiers subIdentLength
                    return (fmap (foo channels) subCodec)
                else return Nothing

        foo :: Int -> Codec -> Codec
        foo channels (Codec a) = reifyIntegral channels (\n -> Codec (NList n [a]))

----------------------------------------------------------------------

-- | Create a track map for a stream of a given type, as track no. 1
oneTrackMultichannel :: (ZoomReadable a)
                     => Int -> a -> Bool -> Bool -> SampleRateType -> Rational -> ByteString -> TrackMap
oneTrackMultichannel channels a delta zlib !drType !rate !name =
    IM.singleton 1 (mkTrackSpecMultichannel channels a delta zlib drType rate name)
{-# INLINABLE oneTrackMultichannel #-}

mkTrackSpecMultichannel :: (ZoomReadable a)
                        => Int -> a -> Bool -> Bool -> SampleRateType -> Rational -> ByteString
                        -> TrackSpec
mkTrackSpecMultichannel channels a = reifyIntegral channels
    (\n -> TrackSpec (Codec (NList n [a])))

----------------------------------------------------------------------

instance (ZoomWrite a, ZoomWritable a) => ZoomWrite [a] where
    write = writeList

writeList :: (ZoomWrite a, ZoomWritable a) => TrackNo -> [a] -> ZoomW ()
writeList tn xs = reifyIntegral (length xs) (\n -> write tn (NList n xs))

instance (ZoomWrite a, ZoomWritable a) => ZoomWrite (SampleOffset, [a]) where
    write = writeSOList

writeSOList :: (ZoomWrite a, ZoomWritable a) => TrackNo -> (SampleOffset, [a]) -> ZoomW ()
writeSOList tn (ts, xs) = reifyIntegral (length xs) (\n -> write tn (ts, (NList n xs)))

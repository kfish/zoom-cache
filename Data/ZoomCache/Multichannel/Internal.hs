{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      : Data.ZoomCache.Multichannel.Internal
-- Copyright   : Conrad Parker
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Conrad Parker <conrad@metadecks.org>
-- Stability   : unstable
-- Portability : unknown
--
-- ZoomCache multichannel API
----------------------------------------------------------------------

module Data.ZoomCache.Multichannel.Internal (
      supportMultichannel
    , identifyCodecMultichannel
    , oneTrackMultichannel
    , mkTrackSpecMultichannel
) where

import Data.ByteString (ByteString)
import Data.Functor.Identity
import qualified Data.IntMap as IM
import qualified Data.Iteratee as I
import Data.TypeLevel.Num hiding ((==))

import qualified Data.Iteratee.Offset as OffI
import Data.Iteratee.ZoomCache.Utils
import Data.Offset
import Data.ZoomCache.Common
import Data.ZoomCache.Multichannel.Common
import Data.ZoomCache.Multichannel.NList()
import Data.ZoomCache.NList
import Data.ZoomCache.Types

----------------------------------------------------------------------

supportMultichannel :: [IdentifyCodec] -> [IdentifyCodec]
supportMultichannel = f
    where f x = x ++ [identifyCodecMultichannel (f x)]

runner1 :: Identity (I.Iteratee s Identity c) -> c
runner1 = runIdentity . I.run . runIdentity

identifyCodecMultichannel :: [IdentifyCodec] -> IdentifyCodec
identifyCodecMultichannel identifiers bs = runner1 $ I.enumPure1Chunk (Offset 0 bs) identifyMulti
    where
        identifyMulti :: (Functor m, Monad m) => I.Iteratee (Offset ByteString) m (Maybe Codec)
        identifyMulti = do
            mIdent <- OffI.takeBS 8
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
{-# DEPRECATED oneTrackMultichannel "Use setCodecMultichannel instead" #-}

mkTrackSpecMultichannel :: (ZoomReadable a)
                        => Int -> a -> Bool -> Bool -> SampleRateType -> Rational -> ByteString
                        -> TrackSpec
mkTrackSpecMultichannel channels a = reifyIntegral channels
    (\n -> TrackSpec (Codec (NList n [a])))
{-# DEPRECATED mkTrackSpecMultichannel "Use setCodecMultichannel instead" #-}

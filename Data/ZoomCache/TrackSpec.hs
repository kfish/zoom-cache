{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
-- |
-- Module      : Data.ZoomCache.TrackSpec
-- Copyright   : Conrad Parker
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Conrad Parker <conrad@metadecks.org>
-- Stability   : unstable
-- Portability : unknown
--
-- ZoomCache track specification
----------------------------------------------------------------------

module Data.ZoomCache.TrackSpec (
    -- * TrackSpec helpers
      setCodec
    , setCodecMultichannel
    , mkTrackSpec
    , oneTrack
) where

import Data.ByteString (ByteString)
import Data.Default
import qualified Data.IntMap as IM
import Data.TypeLevel.Num hiding ((==))

import Data.ZoomCache.Common
import Data.ZoomCache.NList
import Data.ZoomCache.Types

import Data.ZoomCache.Multichannel.NList()
import Data.ZoomCache.Numeric.IEEE754()

------------------------------------------------------------

instance Default TrackSpec where
    def = TrackSpec
        { specType = Codec (undefined :: Double)
        , specDeltaEncode = True
        , specZlibCompress = True
        , specSRType = ConstantSR
        , specRate = 1000
        , specName = ""
        } 

------------------------------------------------------------

-- | Create a track map for a stream of a given type, as track no. 1
oneTrack :: (ZoomReadable a)
         => a -> Bool -> Bool -> SampleRateType -> Rational -> ByteString
         -> TrackMap
oneTrack a delta zlib !drType !rate !name =
    IM.singleton 1 (mkTrackSpec a delta zlib drType rate name)
{-# INLINABLE oneTrack #-}
{-# DEPRECATED oneTrack "Use setCodec instead" #-}

mkTrackSpec :: (ZoomReadable a)
            => a -> Bool -> Bool -> SampleRateType -> Rational -> ByteString
            -> TrackSpec
mkTrackSpec a = TrackSpec (Codec a)
{-# DEPRECATED mkTrackSpec "Use setCodec instead" #-}

setCodec :: ZoomReadable a => a -> TrackSpec -> TrackSpec
setCodec a t = t { specType = Codec a }

setCodecMultichannel :: ZoomReadable a
                     => Int -> a -> TrackSpec -> TrackSpec
setCodecMultichannel channels a t = reifyIntegral channels
    (\n -> t { specType = Codec (NList n [a]) })


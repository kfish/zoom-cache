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
) where

import Data.Default

import Data.ZoomCache.Common
import Data.ZoomCache.Types

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


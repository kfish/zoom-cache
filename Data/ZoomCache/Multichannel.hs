{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      : Data.ZoomCache.Multichannel
-- Copyright   : Conrad Parker
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Conrad Parker <conrad@metadecks.org>
-- Stability   : unstable
-- Portability : unknown
--
-- ZoomCache multichannel API
----------------------------------------------------------------------

module Data.ZoomCache.Multichannel (
      trackTypeMultichannel

    , module Data.ZoomCache.Multichannel.Internal

    -- * Multichannel codecs
    , module Data.ZoomCache.Multichannel.List
) where

import Data.ZoomCache.Multichannel.Internal
import Data.ZoomCache.Multichannel.Common
import Data.ZoomCache.Multichannel.List

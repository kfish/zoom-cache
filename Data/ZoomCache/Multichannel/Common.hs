{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      : Data.ZoomCache.Multichannel.Common
-- Copyright   : Conrad Parker
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Conrad Parker <conrad@metadecks.org>
-- Stability   : unstable
-- Portability : unknown
--
-- ZoomCache multichannel API
----------------------------------------------------------------------

module Data.ZoomCache.Multichannel.Common (
      trackTypeMultichannel
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C

----------------------------------------------------------------------

-- Identifier for track headers
trackTypeMultichannel :: ByteString
trackTypeMultichannel = C.pack "ZOOMmchn"


{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      : Data.ZoomCache.Identify
-- Copyright   : Conrad Parker
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Conrad Parker <conrad@metadecks.org>
-- Stability   : unstable
-- Portability : unknown
--
-- Identifiers for track types of ZoomCache files.
----------------------------------------------------------------------

module Data.ZoomCache.Identify (
    -- * Identification
      identifyCodec
) where

import Data.ZoomCache.Types

----------------------------------------------------------------------

-- | Generate an 'IdentifyTrack' function for a given type.
identifyCodec :: ZoomReadable a => a -> IdentifyCodec
identifyCodec a h
    | h == trackIdentifier a = Just (Codec a)
    | otherwise              = Nothing


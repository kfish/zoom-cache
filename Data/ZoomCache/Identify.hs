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
      identifyTrackType

    -- * Standard mappings
    , standardIdentifiers
) where

import Data.ZoomCache.Types

-- Track Types
import Data.ZoomCache.Double()
import Data.ZoomCache.Int()
import Data.ZoomCache.Unit()

----------------------------------------------------------------------

identifyTrackType :: ZoomReadable a => a -> IdentifyTrack
identifyTrackType a h
    | h == trackIdentifier a = Just (TT a)
    | otherwise              = Nothing

standardIdentifiers :: [IdentifyTrack]
standardIdentifiers =
    [ identifyTrackType (undefined :: Double)
    , identifyTrackType (undefined :: Int)
    , identifyTrackType (undefined :: ())
    ]


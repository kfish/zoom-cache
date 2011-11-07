{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      : Data.ZoomCache.Write
-- Copyright   : Conrad Parker
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Conrad Parker <conrad@metadecks.org>
-- Stability   : unstable
-- Portability : unknown
--
-- Types used throughout zoom-cache
----------------------------------------------------------------------

module Data.ZoomCache.Common (
    -- * Types
      TimeStamp(..)
    , TimeStampDiff(..)
    , DataRateType(..)
    , TrackNo

    -- * Global header
    , Global(..)

    -- * Version
    , Version(..)
) where

import Data.Int

------------------------------------------------------------

type TrackNo = Int

data TimeStamp = TS { unTS :: {-# UNPACK #-}!Int64 }
    deriving (Eq, Ord, Show)

data TimeStampDiff = TSDiff { unTSDiff :: {-# UNPACK #-}!Int64 }
    deriving (Eq, Ord, Show)

data Version = Version !Int !Int
    deriving (Eq, Show)

data Global = Global
    { version          :: Version
    , noTracks         :: Int
    , presentationTime :: Rational
    , baseTime         :: Rational
    , baseUTC          :: Maybe Int -- UTCTime
    }
    deriving (Show)

-- | Constant or Variable datarate.
-- For constant datarate, timestamps are implied as incrementing by 1/datarate
-- For variable datarate, explicit timestamps are attached to each datum, encoded
-- as a separate block of timestamps in the Raw Data packet.
data DataRateType = ConstantDR | VariableDR
    deriving (Show)


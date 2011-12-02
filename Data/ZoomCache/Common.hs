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
    -- * SampleOffsets
      SampleOffset(..)
    , SampleOffsetDiff(..)
    , sampleOffsetDiff

    -- * Types
    , SampleRateType(..)
    , TrackNo

    -- * Global header
    , Global(..)

    -- * Version
    , Version(..)
) where

import Data.Int

------------------------------------------------------------

type TrackNo = Int

data SampleOffset = SO { unSO :: {-# UNPACK #-}!Int64 }
    deriving (Eq, Ord, Show)

data SampleOffsetDiff = SODiff { unSODiff :: {-# UNPACK #-}!Int64 }
    deriving (Eq, Ord, Show)

-- | @sampleOffsetDiff (SO t1) (SO t2) = SODiff (t1 - t2)@
sampleOffsetDiff :: SampleOffset -> SampleOffset -> SampleOffsetDiff
sampleOffsetDiff (SO t1) (SO t2) = SODiff (t1 - t2)

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

-- | Constant or Variable samplerate.
-- For constant samplerate, timestamps are implied as incrementing by 1/samplerate
-- For variable samplerate, explicit timestamps are attached to each datum, encoded
-- as a separate block of 'SampleOffset' in the Raw Data packet.
data SampleRateType = ConstantSR | VariableSR
    deriving (Eq, Show)


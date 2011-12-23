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
    -- * TimeStamps
      TimeStamp (..)
    , TimeStampDiff(..)
    , timeStampDiff
    , timeStampFromSO

    , timeStampFromUTCTime
    , utcTimeFromTimeStamp

    -- * SampleOffsets
    , SampleOffset(..)
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
import Data.Ratio
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime)

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
    , baseUTC          :: Maybe UTCTime
    }

-- | Constant or Variable samplerate.
-- For constant samplerate, timestamps are implied as incrementing by 1/samplerate
-- For variable samplerate, explicit timestamps are attached to each datum, encoded
-- as a separate block of 'SampleOffset' in the Raw Data packet.
data SampleRateType = ConstantSR | VariableSR
    deriving (Eq, Show)

newtype TimeStamp = TS Double
    deriving (Eq, Ord, Show)

newtype TimeStampDiff = TSDiff Double

-- | @timeStampDiff (TS t1) (TS t2) = TSDiff (t1 - t2)@
timeStampDiff :: TimeStamp -> TimeStamp -> TimeStampDiff
timeStampDiff (TS t1) (TS t2) = TSDiff (t1 - t2)

timeStampFromSO :: Rational -> SampleOffset -> TimeStamp
timeStampFromSO r (SO so)
    | n == 0    = TS 0.0
    | otherwise = TS (fromIntegral so * d / n)
    where
          n = fromIntegral $ numerator r
          d = fromIntegral $ denominator r

-- | @utcTimeFromTimeStamp base (TS ts) = baseUTC + ts@
utcTimeFromTimeStamp :: UTCTime -> TimeStamp -> UTCTime
utcTimeFromTimeStamp base (TS ts) = addUTCTime dTime base
    where
        dTime = fromRational . toRational $ ts

-- | @timeStampFromUTCTime base u = TS (u - base)
timeStampFromUTCTime :: UTCTime -> UTCTime -> TimeStamp
timeStampFromUTCTime base u = TS (fromRational . toRational $ d)
    where
        d = diffUTCTime u base

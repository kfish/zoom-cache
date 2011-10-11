{-# LANGUAGE RecordWildCards #-}
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
-- ZoomCache Summary definition
----------------------------------------------------------------------

module Data.ZoomCache.Summary (
  -- * Types
    Summary(..)
  , summaryDuration
  , appendSummary
) where

import Data.ZoomCache.Common

data Summary = SummaryDouble
    { summaryTrack :: TrackNo
    , summaryLevel :: Int
    , summaryEntryTime :: TimeStamp
    , summaryExitTime :: TimeStamp
    , summaryDoubleEntry :: Double
    , summaryDoubleExit :: Double
    , summaryDoubleMin :: Double
    , summaryDoubleMax :: Double
    , summaryAvg :: Double
    , summaryRMS :: Double
    }
    | SummaryInt
    { summaryTrack :: TrackNo
    , summaryLevel :: Int
    , summaryEntryTime :: TimeStamp
    , summaryExitTime :: TimeStamp
    , summaryIntEntry :: Int
    , summaryIntExit :: Int
    , summaryIntMin :: Int
    , summaryIntMax :: Int
    , summaryAvg :: Double
    , summaryRMS :: Double
    }
    deriving (Show)

summaryDuration :: Summary -> Int
summaryDuration s = (unTS $ summaryExitTime s) - (unTS $ summaryEntryTime s)

-- XXX: summaries are only compatible if tracks and levels are equal
appendSummary :: Summary -> Summary -> Summary
appendSummary s1@SummaryDouble{} s2@SummaryDouble{} = SummaryDouble
    { summaryTrack = summaryTrack s1
    , summaryLevel = summaryLevel s1
    , summaryEntryTime = summaryEntryTime s1
    , summaryExitTime = summaryExitTime s2
    , summaryDoubleEntry = summaryDoubleEntry s1
    , summaryDoubleExit = summaryDoubleExit s2
    , summaryDoubleMin = min (summaryDoubleMin s1) (summaryDoubleMin s2)
    , summaryDoubleMax = max (summaryDoubleMax s1) (summaryDoubleMax s2)
    , summaryAvg = ((summaryAvg s1 * dur s1) +
                    (summaryAvg s2 * dur s2)) /
                   (dur s1 + dur s2)
    , summaryRMS = sqrt $ ((summaryRMS s1 * summaryRMS s1 * dur s1) +
                           (summaryRMS s2 * summaryRMS s2 * dur s2)) /
                          (dur s1 + dur s2)
    }
    where
        dur = fromIntegral . summaryDuration
appendSummary s1@SummaryInt{} s2@SummaryInt{} = SummaryInt
    { summaryTrack = summaryTrack s1
    , summaryLevel = summaryLevel s1
    , summaryEntryTime = summaryEntryTime s1
    , summaryExitTime = summaryExitTime s2
    , summaryIntEntry = summaryIntEntry s1
    , summaryIntExit = summaryIntExit s2
    , summaryIntMin = min (summaryIntMin s1) (summaryIntMin s2)
    , summaryIntMax = max (summaryIntMax s1) (summaryIntMax s2)
    , summaryAvg = ((summaryAvg s1 * dur s1) +
                    (summaryAvg s2 * dur s2)) /
                   (dur s1 + dur s2)
    , summaryRMS = sqrt $ ((summaryRMS s1 * summaryRMS s1 * dur s1) +
                           (summaryRMS s2 * summaryRMS s2 * dur s2)) /
                          (dur s1 + dur s2)
    }
    where
        dur = fromIntegral . summaryDuration
appendSummary _ _ = error "Incompatible summaries"

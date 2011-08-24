{-# OPTIONS -Wall #-}

module Zoom.Summary (
  -- * Types
    Summary(..)
) where

import Zoom.Common

data Summary = Summary
    { summaryTrack :: ZoomTrackNo
    , summaryEntryTime :: Int
    , summaryExitTime :: Int
    , summaryLength :: Int
    , summaryEntry :: Double
    , summaryExit :: Double
    , summaryMin :: Double
    , summaryMax :: Double
    , summaryAvg :: Double
    , summaryRMS :: Double
    }


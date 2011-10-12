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
-- Pretty-printing of zoom-cache types
----------------------------------------------------------------------

module Data.ZoomCache.Pretty (
      prettyGlobal
    , prettyTrackSpec
    , prettySummary
) where

import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Ratio
import Text.Printf

import Data.ZoomCache.Common
import Data.ZoomCache.Summary

----------------------------------------------------------------------

-- | Pretty-print a 'Global'
prettyGlobal :: Global -> String
prettyGlobal Global{..} = unlines
    [ "Version:\t\t" ++ show vMaj ++ "." ++ show vMin
    , "No. tracks:\t\t" ++ show noTracks
    , "Presentation-time:\t" ++ ratShow presentationTime
    , "Base-time:\t\t" ++ ratShow baseTime
    , "UTC baseTime:\t\t" ++ maybe "undefined" show baseUTC
    ]
    where
        Version vMaj vMin = version

-- | Pretty-print a 'TrackSpec'
prettyTrackSpec :: TrackNo -> TrackSpec -> String
prettyTrackSpec trackNo TrackSpec{..} = unlines
    [ "Track " ++ show trackNo ++ ":"
    , "\tName:\t" ++ LC.unpack specName
    , "\tType:\t" ++ show specType
    , "\tRate:\t" ++ show specDRType ++ " " ++ ratShow specRate
    ]

-- | Pretty-print a 'Summary'
prettySummary :: Summary -> String
prettySummary s@SummaryDouble{..} = concat
    [ prettySummaryTimes s
    , prettySummaryLevel s
    , printf "\tentry: %.3f\texit: %.3f\tmin: %.3f\tmax: %.3f\t"
          summaryDoubleEntry summaryDoubleExit summaryDoubleMin summaryDoubleMax
    , prettySummaryAvgRMS s
    ]
prettySummary s@SummaryInt{..} = concat
    [ prettySummaryTimes s
    , prettySummaryLevel s
    , printf "\tentry: %d\texit: %df\tmin: %d\tmax: %d\t"
        summaryIntEntry summaryIntExit summaryIntMin summaryIntMax
    , prettySummaryAvgRMS s
    ]

prettySummaryTimes :: Summary -> String
prettySummaryTimes s = printf "[%d - %d]" (unTS $ summaryEntryTime s)
                                          (unTS $ summaryExitTime s)

prettySummaryLevel :: Summary -> String
prettySummaryLevel s = printf "lvl: %d" (summaryLevel s)

prettySummaryAvgRMS :: Summary -> String
prettySummaryAvgRMS s = printf "avg: %.3f\trms: %.3f" (summaryAvg s) (summaryRMS s)

----------------------------------------------------------------------

ratShow :: Rational -> String
ratShow r
    | d == 0 = "0"
    | d == 1 = show n
    | otherwise = show n ++ "/" ++ show d
    where
        n = numerator r
        d = denominator r


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
    , prettyTimeStamp
    , prettySampleOffset
    , prettySummarySO
) where

import qualified Data.ByteString.Char8 as C
import Data.Ratio
import Data.Time (UTCTime, formatTime)
import System.Locale (defaultTimeLocale)
import Text.Printf

import Data.ZoomCache.Common
import Data.ZoomCache.Types

----------------------------------------------------------------------

-- | Pretty-print a 'Global'
prettyGlobal :: Global -> String
prettyGlobal Global{..} = unlines
    [ "Version:\t\t" ++ show vMaj ++ "." ++ show vMin
    , "No. tracks:\t\t" ++ show noTracks
    , "UTC baseTime:\t\t" ++ maybe "undefined" showUTC baseUTC
    ]
    where
        Version vMaj vMin = version
        showUTC :: UTCTime -> String
        showUTC = formatTime defaultTimeLocale "%Y-%m-%d %T"

-- | Pretty-print a 'TrackSpec'
prettyTrackSpec :: TrackNo -> TrackSpec -> String
prettyTrackSpec trackNo TrackSpec{..} = unlines
    [ "Track " ++ show trackNo ++ ":"
    , "\tName:\t" ++ C.unpack specName
    , "\tType:\t" ++ show specType
    , "\tEnc:\t"  ++ unwords [encoding, compression]
    , "\tRate:\t" ++ show specSRType ++ " " ++ ratShow specRate
    ]
    where
        encoding | specDeltaEncode = "Delta"
                 | otherwise       = "Raw"
        compression | specZlibCompress = "Zlib"
                    | otherwise        = "Uncompressed"

-- | Pretty-print a 'SampleOffset', given a datarate
prettyTimeStamp :: TimeStamp -> String
prettyTimeStamp (TS ts) = printf "%02d:%02d:%02d.%03d" hrs minN secN msN
    where
          secT, msN :: Integer
          secT = floor ts
          msN = round (1000 * (ts - fromIntegral secT))
          (minT, secN) = quotRem secT 60
          (hrs, minN) = quotRem minT 60

-- | Pretty-print a 'SampleOffset', given a datarate
prettySampleOffset :: Rational -> SampleOffset -> String
prettySampleOffset r (SO t)
    | d == 0    = "00:00:00.000"
    {-
    | d < 100   = printf "%02d:%02d:%02d::%02d" hrs minN secN framesN
    -}
    | otherwise = printf "%02d:%02d:%02d.%03d" hrs minN secN msN
    where
          d = denominator r
          n = numerator r
          msN = quot (1000 * framesN) n
          (secT, framesN) = quotRem (fromIntegral t*d) n
          (minT, secN) = quotRem secT 60
          (hrs, minN) = quotRem minT 60

-- | Pretty-print a 'SummarySO', given a datarate
prettySummarySO :: ZoomReadable a => Rational -> SummarySO a -> String
prettySummarySO r s = concat
    [ prettySummarySOTimes r s
    , prettySummarySOLevel s
    , prettySummaryData (summarySOData s)
    ]

prettySummarySOTimes :: Rational -> SummarySO a -> String
prettySummarySOTimes r s = concat
    [ "[", (prettySampleOffset r $ summarySOEntry s)
    , "-", (prettySampleOffset r $ summarySOExit s), "] "
    ]

prettySummarySOLevel :: SummarySO a -> String
prettySummarySOLevel s = printf "lvl: %d" (summarySOLevel s)

----------------------------------------------------------------------

ratShow :: Rational -> String
ratShow r
    | d == 0 = "0"
    | d == 1 = show n
    | otherwise = show n ++ "/" ++ show d
    where
        n = numerator r
        d = denominator r


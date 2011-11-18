{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall #-}

module Data.ZoomCache.Numeric.Types (
    -- * Summaries of numeric values
      ZoomNum(..)
) where

import Data.ZoomCache.Codec

----------------------------------------------------------------------
-- ZoomNum

class (Ord a, Real a, ZoomReadable a, ZoomWritable a) => ZoomNum a where
    numEntry :: SummaryData a -> a
    numExit  :: SummaryData a -> a
    numMin   :: SummaryData a -> a
    numMax   :: SummaryData a -> a
    numAvg   :: SummaryData a -> Double
    numRMS   :: SummaryData a -> Double

    numWorkTime  :: SummaryWork a -> TimeStamp
    numWorkEntry :: SummaryWork a -> Maybe a
    numWorkExit  :: SummaryWork a -> a
    numWorkMin   :: SummaryWork a -> a
    numWorkMax   :: SummaryWork a -> a
    numWorkSum   :: SummaryWork a -> Double
    numWorkSumSq :: SummaryWork a -> Double

    numMkSummary :: a -> a -> a -> a -> Double -> Double -> SummaryData a
    numMkSummaryWork :: TimeStamp -> Maybe a -> a -> a -> a -> Double -> Double
                     -> SummaryWork a

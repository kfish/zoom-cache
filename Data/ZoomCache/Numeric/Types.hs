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
    -- | Value at start of interval
    numEntry :: SummaryData a -> a
    -- | Value at end of interval
    numExit  :: SummaryData a -> a

    -- | Minimum value in the summary interval
    numMin   :: SummaryData a -> a
    -- | Maximum value in the summary interval
    numMax   :: SummaryData a -> a

    -- | Mean value in the summary interval
    numAvg   :: SummaryData a -> Double

    -- | Root mean square value in the summary interval
    numRMS   :: SummaryData a -> Double

    numWorkSO    :: SummaryWork a -> SampleOffset
    numWorkEntry :: SummaryWork a -> Maybe a
    numWorkExit  :: SummaryWork a -> a
    numWorkMin   :: SummaryWork a -> a
    numWorkMax   :: SummaryWork a -> a
    numWorkSum   :: SummaryWork a -> Double
    numWorkSumSq :: SummaryWork a -> Double

    numMkSummary :: a -> a -> a -> a -> Double -> Double -> SummaryData a
    numMkSummaryWork :: SampleOffset -> Maybe a -> a -> a -> a -> Double -> Double
                     -> SummaryWork a

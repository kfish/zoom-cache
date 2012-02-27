{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall #-}

module Data.ZoomCache.Numeric.Internal (
    -- * Functions
      readSummaryNum
    , fromSummaryNum
    , initSummaryNumBounded
    , mkSummaryNum
    , appendSummaryNum
    , updateSummaryNum
    , deltaDecodeNum
    , deltaEncodeNum
) where

import Blaze.ByteString.Builder
import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import Data.Iteratee (Iteratee)
import Data.Maybe (fromMaybe)
import Data.Monoid

import Data.ZoomCache.Codec
import Data.ZoomCache.Numeric.Types

----------------------------------------------------------------------

readSummaryNum :: (Functor m, Monad m, ZoomNum a)
               => Iteratee ByteString m (SummaryData a)
readSummaryNum = do
    [!en,!ex,!mn,!mx] <- replicateM 4 readRaw
    [!avg,!rms] <- replicateM 2 readDouble64be
    return $! numMkSummary en ex mn mx avg rms
{-# INLINE readSummaryNum #-}

fromSummaryNum :: ZoomNum a
               => SummaryData a -> Builder
fromSummaryNum s = mconcat $
    map fromRaw [numEntry s, numExit s, numMin s, numMax s] ++
    map fromDouble [numAvg s, numRMS s]
{-# INLINE fromSummaryNum #-}

initSummaryNumBounded :: (Bounded a, ZoomNum a)
                      => SampleOffset -> SummaryWork a
initSummaryNumBounded entry = numMkSummaryWork entry Nothing 0 maxBound minBound 0.0 0.0
{-# INLINEABLE initSummaryNumBounded #-}

mkSummaryNum :: ZoomNum a
             => SampleOffsetDiff -> SummaryWork a
             -> SummaryData a
mkSummaryNum (SODiff dur) sw =
    numMkSummary (fromMaybe 0 $ numWorkEntry sw) (numWorkExit sw)
                 (numWorkMin sw) (numWorkMax sw)
                 (numWorkSum sw / fromIntegral dur)
                 (sqrt $ (numWorkSumSq sw) / fromIntegral dur)
{-# INLINEABLE mkSummaryNum #-}

appendSummaryNum :: ZoomNum a
                 => SampleOffsetDiff -> SummaryData a
                 -> SampleOffsetDiff -> SummaryData a
                 -> SummaryData a
appendSummaryNum (SODiff dur1) s1 (SODiff dur2) s2 = numMkSummary
    (numEntry s1)
    (numExit s2)
    (min (numMin s1) (numMin s2))
    (max (numMax s1) (numMax s2))
    (((numAvg s1 * fromIntegral dur1) + (numAvg s2 * fromIntegral dur2)) / fromIntegral durSum)
    (sqrt $ ((numRMS s1 * numRMS s1 * fromIntegral dur1) +
             (numRMS s2 * numRMS s2 * fromIntegral dur2)) /
            fromIntegral durSum)
    where
        !durSum = dur1 + dur2
{-# INLINEABLE appendSummaryNum #-}

updateSummaryNum :: ZoomNum a
                 => SampleOffset -> a
                 -> SummaryWork a
                 -> SummaryWork a
updateSummaryNum t d sw =
    numMkSummaryWork t (Just $ fromMaybe d (numWorkEntry sw))
                       d
                       (min (numWorkMin sw) d)
                       (max (numWorkMax sw) d)
                       ((numWorkSum sw) + realToFrac (d * fromIntegral dur))
                       ((numWorkSumSq sw) + realToFrac (d*d * fromIntegral dur))
    where
        !(SODiff dur) = sampleOffsetDiff t (numWorkSO sw)
{-# INLINEABLE updateSummaryNum #-}

deltaDecodeNum :: ZoomNum a => [a] -> [a]
deltaDecodeNum = deltaDecode
{-# INLINE deltaDecodeNum #-}

deltaEncodeNum :: ZoomNum a => SummaryWork a -> a -> a
deltaEncodeNum sw d = d - numWorkExit sw
{-# INLINE deltaEncodeNum #-}

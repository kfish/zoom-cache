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
import Data.Iteratee (Iteratee)
import qualified Data.Iteratee as I
import qualified Data.ListLike as LL
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Word

import Data.ZoomCache.Codec
import Data.ZoomCache.Numeric.Types

----------------------------------------------------------------------

readSummaryNum :: (I.Nullable s, LL.ListLike s Word8,
                   Functor m, Monad m,
                   ZoomNum a)
               => Iteratee s m (SummaryData a)
readSummaryNum = do
    [en,ex,mn,mx] <- replicateM 4 readRaw
    [avg,rms] <- replicateM 2 readDouble64be
    return (numMkSummary en ex mn mx avg rms)
{-# INLINABLE readSummaryNum #-}

fromSummaryNum :: ZoomNum a
               => SummaryData a -> Builder
fromSummaryNum s = mconcat $
    map fromRaw [numEntry s, numExit s, numMin s, numMax s] ++
    map fromDouble [numAvg s, numRMS s]
{-# INLINABLE fromSummaryNum #-}

initSummaryNumBounded :: (Bounded a, ZoomNum a)
                      => TimeStamp -> SummaryWork a
initSummaryNumBounded entry = numMkSummaryWork entry Nothing 0 maxBound minBound 0.0 0.0
{-# INLINEABLE initSummaryNumBounded #-}

mkSummaryNum :: ZoomNum a
             => TimeStampDiff -> SummaryWork a
             -> SummaryData a
mkSummaryNum (TSDiff dur) sw =
    numMkSummary (fromMaybe 0 $ numWorkEntry sw) (numWorkExit sw)
                 (numWorkMin sw) (numWorkMax sw)
                 (numWorkSum sw / fromIntegral dur)
                 (sqrt $ (numWorkSumSq sw) / fromIntegral dur)
{-# INLINEABLE mkSummaryNum #-}

appendSummaryNum :: ZoomNum a
                 => TimeStampDiff -> SummaryData a
                 -> TimeStampDiff -> SummaryData a
                 -> SummaryData a
appendSummaryNum (TSDiff dur1) s1 (TSDiff dur2) s2 = numMkSummary
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
                 => TimeStamp -> a
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
        !(TSDiff dur) = timeStampDiff t (numWorkTime sw)
{-# INLINEABLE updateSummaryNum #-}

deltaDecodeNum :: ZoomNum a => [a] -> [a]
deltaDecodeNum = scanl1 (+)

deltaEncodeNum :: ZoomNum a => SummaryWork a -> a -> a
deltaEncodeNum sw d = d - numWorkExit sw

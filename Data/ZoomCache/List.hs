{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
{- |
   Module      : Data.ZoomCache.List
   Copyright   : Conrad Parker
   License     : BSD3-style (see LICENSE)

   Maintainer  : Conrad Parker <conrad@metadecks.org>
   Stability   : unstable
   Portability : unknown

Default codec implementation for multichannel values of type [a].

This module implements the interfaces documented in "Data.ZoomCache.Codec".

The table below describes the encoding of SummaryData for List.

@
   | ...                                                           |   -35
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Expected value (double)                                       | 36-39
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | ...                                                           | 40-43
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
@

Field encoding formats:

  @double@: big-endian IEEE 754-2008 binary64 (IEEE 754-1985 double)

-}
----------------------------------------------------------------------

module Data.ZoomCache.List (
      SummaryData(..)
    , SummaryWork(..)
)where

import Blaze.ByteString.Builder
import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import Data.Iteratee (Iteratee)
import Data.Monoid (mconcat)

import Data.ZoomCache.Codec

----------------------------------------------------------------------

-- Identifier for track headers
trackTypeNList :: ByteString
trackTypeNList = "ZOOMmchn"

nChannelsLOL :: Int
nChannelsLOL = 8

----------------------------------------------------------------------
-- Read

instance ZoomReadable a => ZoomReadable [a] where
    data SummaryData [a] = SummaryNList [SummaryData a]

    trackIdentifier = mkTrackTypeNList

    readRaw     = readNList
    readSummary = readSummaryNList

    prettyRaw         = prettyPacketNList
    prettySummaryData = prettySummaryNList

mkTrackTypeNList :: ZoomReadable a => [a] -> ByteString
mkTrackTypeNList l = mconcat
    [ trackTypeNList
    , toByteString . fromIntegral32be . length $ l
    , trackIdentifier (head l)
    ]

prettyPacketNList :: ZoomReadable a => [a] -> String
prettyPacketNList l = show (map prettyRaw l)

readNList :: (Functor m, Monad m, ZoomReadable a)
          => Iteratee ByteString m [a]
readNList = replicateM nChannelsLOL readRaw

readSummaryNList :: (Functor m, Monad m, ZoomReadable a)
                 => Iteratee ByteString m (SummaryData [a])
readSummaryNList = SummaryNList <$> replicateM nChannelsLOL readSummary

prettySummaryNList :: ZoomReadable a => SummaryData [a] -> String
prettySummaryNList (SummaryNList l) = show (map prettySummaryData l)

----------------------------------------------------------------------
-- Write

instance (ZoomWrite a, ZoomWritable a) => ZoomWrite [a] where
    write = writeData

instance (ZoomWrite a, ZoomWritable a) => ZoomWrite (SampleOffset, [a]) where
    write = writeDataVBR

instance ZoomWritable a => ZoomWritable [a] where
    data SummaryWork [a] = SummaryWorkNList [SummaryWork a]

    fromRaw           = fromNList
    fromSummaryData   = fromSummaryNList

    initSummaryWork   = initSummaryNList
    toSummaryData     = mkSummaryNList
    updateSummaryData = updateSummaryNList
    appendSummaryData = appendSummaryNList

fromNList :: ZoomWritable a => [a] -> Builder
fromNList = mconcat . map fromRaw

initSummaryNList :: ZoomWritable a => SampleOffset -> SummaryWork [a]
initSummaryNList entry = SummaryWorkNList (replicate nChannelsLOL (initSummaryWork entry))

mkSummaryNList :: ZoomWritable a => SampleOffsetDiff -> SummaryWork [a] -> SummaryData [a]
mkSummaryNList dur (SummaryWorkNList l) = SummaryNList (map (toSummaryData dur) l)

fromSummaryNList :: ZoomWritable a => SummaryData [a] -> Builder
fromSummaryNList (SummaryNList l) = mconcat $ map fromSummaryData l

updateSummaryNList :: ZoomWritable a
                   => SampleOffset  -> [a] -> SummaryWork [a]
                   -> SummaryWork [a]
updateSummaryNList t xs (SummaryWorkNList ws) =
    SummaryWorkNList (zipWith (updateSummaryData t) xs ws)

appendSummaryNList :: ZoomWritable a
                   => SampleOffsetDiff -> SummaryData [a]
                   -> SampleOffsetDiff -> SummaryData [a]
                   -> SummaryData [a]
appendSummaryNList dur1 (SummaryNList l1) dur2 (SummaryNList l2) =
    SummaryNList (zipWith a l1 l2)
    where
        a s1 s2 = appendSummaryData dur1 s1 dur2 s2

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
{- |
   Module      : Data.ZoomCache.Unit
   Copyright   : Conrad Parker
   License     : BSD3-style (see LICENSE)

   Maintainer  : Conrad Parker <conrad@metadecks.org>
   Stability   : unstable
   Portability : unknown

Default codec implementation for values of type (). Elements of type ()
are useful for marking events, and variable rate tracks written with
type (SampleOffset, ()) are useful for recording times of events.

This module implements the interfaces documented in "Data.ZoomCache.Codec".

As elements of type () contain no unique information it is sufficient to
record only a count of elements which occur within each packet.

No raw data is encoded for tracks of type () as the raw data packet header
already includes a count of elements. This is implemented by specifying
const 'mempty') as the 'Builder' and (return ()) as the reader.

The table below describes the encoding of SummaryData for ().

@
   | ...                                                           |   -35
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Count (int32)                                                 | 36-39
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
@

Field encoding formats:

  @int32@:  32bit big endian

-}
----------------------------------------------------------------------

module Data.ZoomCache.Unit (
      SummaryData(..)
    , SummaryWork(..)
)where

import Blaze.ByteString.Builder
import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import Data.Iteratee (Iteratee)
import Data.Monoid
import Text.Printf

import Data.ZoomCache.Codec

----------------------------------------------------------------------

-- Identifier for track headers
trackTypeUnit :: ByteString
trackTypeUnit = "ZOOMunit"

----------------------------------------------------------------------
-- Read

instance ZoomReadable () where
    data SummaryData () = SummaryUnit
        { summaryUnitCount :: {-# UNPACK #-}!Int
        }

    trackIdentifier = const trackTypeUnit

    readRaw     = return ()
    readSummary = readSummaryUnit

    prettyRaw         = prettyPacketUnit
    prettySummaryData = prettySummaryUnit

prettyPacketUnit :: () -> String
prettyPacketUnit = const "."

readSummaryUnit :: (Functor m, Monad m)
               => Iteratee ByteString m (SummaryData ())
readSummaryUnit = SummaryUnit <$> readInt32be

prettySummaryUnit :: SummaryData () -> String
prettySummaryUnit SummaryUnit{..} = printf "count: %d" summaryUnitCount

----------------------------------------------------------------------
-- Write

instance ZoomWrite () where
    write = writeData

instance ZoomWrite (SampleOffset, ()) where
    write = writeDataVBR

instance ZoomWrite (TimeStamp, ()) where
    write = writeDataTS

instance ZoomWritable () where
    data SummaryWork () = SummaryWorkUnit
        { swUnitCount :: {-# UNPACK #-}!Int
        }

    fromRaw           = const mempty
    fromSummaryData   = fromSummaryUnit

    initSummaryWork   = initSummaryUnit
    toSummaryData     = mkSummaryUnit
    updateSummaryData = updateSummaryUnit
    appendSummaryData = appendSummaryUnit

initSummaryUnit :: SampleOffset -> SummaryWork ()
initSummaryUnit _ = SummaryWorkUnit
    { swUnitCount = 0
    }

mkSummaryUnit :: SampleOffsetDiff -> SummaryWork () -> SummaryData ()
mkSummaryUnit _dur SummaryWorkUnit{..} = SummaryUnit
    { summaryUnitCount = swUnitCount
    }

fromSummaryUnit :: SummaryData () -> Builder
fromSummaryUnit SummaryUnit{..} = mconcat $ map fromIntegral32be
    [ summaryUnitCount
    ]

updateSummaryUnit :: SampleOffset  -> () -> SummaryWork ()
                 -> SummaryWork ()
updateSummaryUnit _t _ SummaryWorkUnit{..} = SummaryWorkUnit
    { swUnitCount = swUnitCount + 1
    }

appendSummaryUnit :: SampleOffsetDiff -> SummaryData ()
                  -> SampleOffsetDiff -> SummaryData ()
                  -> SummaryData ()
appendSummaryUnit _dur1 s1 _dur2 s2 = SummaryUnit
    { summaryUnitCount = summaryUnitCount s1 + summaryUnitCount s2
    }


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
type (TimeStamp, ()) are useful for recording times of events.

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
import Control.Monad.Trans (MonadIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Iteratee (Iteratee)
import qualified Data.Iteratee as I
import qualified Data.ListLike as LL
import Data.Monoid
import Data.Word
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

readSummaryUnit :: (I.Nullable s, LL.ListLike s Word8, Functor m, MonadIO m)
               => Iteratee s m (SummaryData ())
readSummaryUnit = do
    count <- readInt32be
    return (SummaryUnit count)
{-# SPECIALIZE INLINE readSummaryUnit :: (Functor m, MonadIO m) => Iteratee [Word8] m (SummaryData ()) #-}
{-# SPECIALIZE INLINE readSummaryUnit :: (Functor m, MonadIO m) => Iteratee B.ByteString m (SummaryData ()) #-}

prettySummaryUnit :: SummaryData () -> String
prettySummaryUnit SummaryUnit{..} =
    printf "count: %d" summaryUnitCount

----------------------------------------------------------------------
-- Write

instance ZoomWrite () where
    write = writeData

instance ZoomWrite (TimeStamp, ()) where
    write = writeDataVBR

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

initSummaryUnit :: TimeStamp -> SummaryWork ()
initSummaryUnit _ = SummaryWorkUnit
    { swUnitCount = 0
    }

mkSummaryUnit :: Double -> SummaryWork () -> SummaryData ()
mkSummaryUnit _dur SummaryWorkUnit{..} = SummaryUnit
    { summaryUnitCount = swUnitCount
    }

fromSummaryUnit :: SummaryData () -> Builder
fromSummaryUnit SummaryUnit{..} = mconcat $ map fromIntegral32be
    [ summaryUnitCount
    ]

updateSummaryUnit :: TimeStamp  -> () -> SummaryWork ()
                 -> SummaryWork ()
updateSummaryUnit _t _ SummaryWorkUnit{..} = SummaryWorkUnit
    { swUnitCount = swUnitCount + 1
    }

appendSummaryUnit :: Double -> SummaryData ()
                  -> Double -> SummaryData ()
                  -> SummaryData ()
appendSummaryUnit _dur1 s1 _dur2 s2 = SummaryUnit
    { summaryUnitCount = summaryUnitCount s1 + summaryUnitCount s2
    }


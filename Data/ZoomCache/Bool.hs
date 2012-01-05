{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
{- |
   Module      : Data.ZoomCache.Bool
   Copyright   : Conrad Parker
   License     : BSD3-style (see LICENSE)

   Maintainer  : Conrad Parker <conrad@metadecks.org>
   Stability   : unstable
   Portability : unknown

Default codec implementation for values of type Bool. Elements of type Bool
are useful for recording observations of binary events.

This module implements the interfaces documented in "Data.ZoomCache.Codec".

The table below describes the encoding of SummaryData for Bool.

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

module Data.ZoomCache.Bool (
      SummaryData(..)
    , SummaryWork(..)
)where

import Blaze.ByteString.Builder
import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import Data.Int
import Data.Iteratee (Iteratee)
import qualified Data.Iteratee as I
import Text.Printf

import Data.ZoomCache.Codec

----------------------------------------------------------------------

-- Identifier for track headers
trackTypeBool :: ByteString
trackTypeBool = "ZOOMbool"

----------------------------------------------------------------------
-- Read

instance ZoomReadable Bool where
    data SummaryData Bool = SummaryBool
        { summaryBoolExpected :: {-# UNPACK #-}!Double
        }

    trackIdentifier = const trackTypeBool

    readRaw     = readBool
    readSummary = readSummaryBool

    prettyRaw         = prettyPacketBool
    prettySummaryData = prettySummaryBool

prettyPacketBool :: Bool -> String
prettyPacketBool = show

readBool :: (Functor m, Monad m)
         => Iteratee ByteString m Bool
readBool = (/= 0) <$> I.head

readSummaryBool :: (Functor m, Monad m)
                => Iteratee ByteString m (SummaryData Bool)
readSummaryBool = SummaryBool <$> readDouble64be

prettySummaryBool :: SummaryData Bool -> String
prettySummaryBool SummaryBool{..} = printf "expected: %.3f" summaryBoolExpected

----------------------------------------------------------------------
-- Write

instance ZoomWrite Bool where
    write = writeData

instance ZoomWrite (SampleOffset, Bool) where
    write = writeDataVBR

instance ZoomWrite (TimeStamp, Bool) where
    write = writeDataTS

instance ZoomWritable Bool where
    data SummaryWork Bool = SummaryWorkBool
        { swBoolSO      :: {-# UNPACK #-}!SampleOffset
        , swBoolTrueTotal :: {-# UNPACK #-}!Int64
        }

    fromRaw           = fromBool
    fromSummaryData   = fromSummaryBool

    initSummaryWork   = initSummaryBool
    toSummaryData     = mkSummaryBool
    updateSummaryData = updateSummaryBool
    appendSummaryData = appendSummaryBool

fromBool :: Bool -> Builder
fromBool False = fromInt8 0
fromBool True  = fromInt8 1

initSummaryBool :: SampleOffset -> SummaryWork Bool
initSummaryBool entry = SummaryWorkBool
    { swBoolSO = entry
    , swBoolTrueTotal = 0
    }

mkSummaryBool :: SampleOffsetDiff -> SummaryWork Bool -> SummaryData Bool
mkSummaryBool (SODiff dur) SummaryWorkBool{..} = SummaryBool
    { summaryBoolExpected = fromIntegral swBoolTrueTotal / fromIntegral dur
    }

fromSummaryBool :: SummaryData Bool -> Builder
fromSummaryBool SummaryBool{..} = fromDouble summaryBoolExpected

updateSummaryBool :: SampleOffset  -> Bool -> SummaryWork Bool
                  -> SummaryWork Bool
updateSummaryBool t False sw = sw { swBoolSO = t }
updateSummaryBool t True SummaryWorkBool{..} = SummaryWorkBool
    { swBoolSO = t
    , swBoolTrueTotal = swBoolTrueTotal + dur
    }
    where
        !(SODiff dur) = sampleOffsetDiff t swBoolSO

appendSummaryBool :: SampleOffsetDiff -> SummaryData Bool
                  -> SampleOffsetDiff -> SummaryData Bool
                  -> SummaryData Bool
appendSummaryBool (SODiff dur1) s1 (SODiff dur2) s2 = SummaryBool
    { summaryBoolExpected = ((summaryBoolExpected s1 * fromIntegral dur1) +
                             (summaryBoolExpected s2 * fromIntegral dur2)) /
                            fromIntegral durSum
    }
    where
        !durSum = dur1 + dur2


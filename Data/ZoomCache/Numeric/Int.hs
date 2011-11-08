{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
{- |
   Module      : Data.ZoomCache.Numeric.Int
   Copyright   : Conrad Parker
   License     : BSD3-style (see LICENSE)

   Maintainer  : Conrad Parker <conrad@metadecks.org>
   Stability   : unstable
   Portability : unknown

Default codec implementation for values of type Int. This module
implements the interfaces documented in "Data.ZoomCache.Codec".
View the module source for enlightenment.

The table below describes the encoding of SummaryData for Int8:

@
   | ...                                                           |   -35
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Entry (int8)  | Exit (int8)   | Min (int8)    | Max (int8)    | 36-39
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Avg (double)                                                  | 40-43
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 44-47
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | RMS (double)                                                  | 48-51
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
@

The table below describes the encoding of SummaryData for Int16:

@
   | ...                                                           |   -35
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Entry (int16)                 | Exit (int16)                  | 36-39
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Min (int16)                   | Max (int16)                   | 40-43
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Avg (double)                                                  | 44-47
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 48-51
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | RMS (double)                                                  | 52-55
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 56-59
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
@

The table below describes the encoding of SummaryData for Int and Int32:

@
   | ...                                                           |   -35
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Entry (int32)                                                 | 36-39
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Exit (int32)                                                  | 40-43
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Min (int32)                                                   | 44-47
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Max (int32)                                                   | 48-51
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Avg (double)                                                  | 52-55
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 56-59
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | RMS (double)                                                  | 60-63
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 64-67
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
@

The table below describes the encoding of SummaryData for Int64:

@
   | ...                                                           |   -35
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Entry (int64)                                                 | 36-39
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 40-43
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Exit (int64)                                                  | 44-47
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 48-51
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Min (int64)                                                   | 52-55
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 56-59
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Max (int64)                                                   | 60-63
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 64-67
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Avg (int64)                                                   | 68-71
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 72-75
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | RMS (int64)                                                   | 76-79
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 80-83
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
@

Field encoding formats:

  @int8@:   8bit signed integer

  @int16@:  16bit big endian signed integer

  @int32@:  32bit big endian signed integer

  @int64@:  32bit big endian signed integer

  @double@: big-endian IEEE 754-2008 binary64 (IEEE 754-1985 double)

-}
----------------------------------------------------------------------

module Data.ZoomCache.Numeric.Int (
      SummaryData(..)
    , SummaryWork(..)
)where

import Blaze.ByteString.Builder
import Control.Monad.Trans (MonadIO)
import Data.ByteString (ByteString)
import Data.Int
import Data.Iteratee (Iteratee)
import Data.Word
import Text.Printf

import Data.ZoomCache.Codec
import Data.ZoomCache.Numeric.Internal
import Data.ZoomCache.Numeric.Types

----------------------------------------------------------------------
-- Int

instance ZoomReadable Int where
    data SummaryData Int = SummaryInt
        { summaryIntEntry :: {-# UNPACK #-}!Int
        , summaryIntExit  :: {-# UNPACK #-}!Int
        , summaryIntMin   :: {-# UNPACK #-}!Int
        , summaryIntMax   :: {-# UNPACK #-}!Int
        , summaryIntAvg   :: {-# UNPACK #-}!Double
        , summaryIntRMS   :: {-# UNPACK #-}!Double
        }

    trackIdentifier = const "ZOOMi32b"

    readRaw     = readInt32be
    readSummary = readSummaryNum

    prettyRaw         = show
    prettySummaryData = prettySummaryInt

{-# SPECIALIZE readSummaryNum :: (Functor m, MonadIO m) => Iteratee [Word8] m (SummaryData Int) #-}
{-# SPECIALIZE readSummaryNum :: (Functor m, MonadIO m) => Iteratee ByteString m (SummaryData Int) #-}

instance ZoomWrite Int where
    write = writeData

instance ZoomWrite (TimeStamp, Int) where
    write = writeDataVBR

instance ZoomWritable Int where
    data SummaryWork Int = SummaryWorkInt
        { swIntTime  :: {-# UNPACK #-}!TimeStamp
        , swIntEntry :: !(Maybe Int)
        , swIntExit  :: {-# UNPACK #-}!Int
        , swIntMin   :: {-# UNPACK #-}!Int
        , swIntMax   :: {-# UNPACK #-}!Int
        , swIntSum   :: {-# UNPACK #-}!Double
        , swIntSumSq :: {-# UNPACK #-}!Double
        }

    fromRaw           = fromIntegral32be
    fromSummaryData   = fromSummaryNum

    initSummaryWork   = initSummaryNumBounded
    toSummaryData     = mkSummaryNum
    updateSummaryData = updateSummaryNum
    appendSummaryData = appendSummaryNum

instance ZoomNum Int where
    numEntry = summaryIntEntry
    numExit = summaryIntExit
    numMin = summaryIntMin
    numMax = summaryIntMax
    numAvg = summaryIntAvg
    numRMS = summaryIntRMS

    numWorkTime = swIntTime
    numWorkEntry = swIntEntry
    numWorkExit = swIntExit
    numWorkMin = swIntMin
    numWorkMax = swIntMax
    numWorkSum = swIntSum
    numWorkSumSq = swIntSumSq

    numMkSummary = SummaryInt
    numMkSummaryWork = SummaryWorkInt

{-# SPECIALIZE fromSummaryNum :: SummaryData Int -> Builder #-}
{-# SPECIALIZE initSummaryNumBounded :: TimeStamp -> SummaryWork Int #-}
{-# SPECIALIZE mkSummaryNum :: TimeStampDiff -> SummaryWork Int -> SummaryData Int #-}
{-# SPECIALIZE appendSummaryNum :: TimeStampDiff -> SummaryData Int -> TimeStampDiff -> SummaryData Int -> SummaryData Int #-}
{-# SPECIALIZE updateSummaryNum :: TimeStamp -> Int -> SummaryWork Int -> SummaryWork Int #-}

----------------------------------------------------------------------
-- Int8

instance ZoomReadable Int8 where
    data SummaryData Int8 = SummaryInt8
        { summaryInt8Entry :: {-# UNPACK #-}!Int8
        , summaryInt8Exit  :: {-# UNPACK #-}!Int8
        , summaryInt8Min   :: {-# UNPACK #-}!Int8
        , summaryInt8Max   :: {-# UNPACK #-}!Int8
        , summaryInt8Avg   :: {-# UNPACK #-}!Double
        , summaryInt8RMS   :: {-# UNPACK #-}!Double
        }

    trackIdentifier = const "ZOOMiS8b"

    readRaw     = readInt8
    readSummary = readSummaryNum

    prettyRaw         = show
    prettySummaryData = prettySummaryInt

{-# SPECIALIZE readSummaryNum :: (Functor m, MonadIO m) => Iteratee [Word8] m (SummaryData Int8) #-}
{-# SPECIALIZE readSummaryNum :: (Functor m, MonadIO m) => Iteratee ByteString m (SummaryData Int8) #-}

instance ZoomWrite Int8 where
    write = writeData

instance ZoomWrite (TimeStamp, Int8) where
    write = writeDataVBR

instance ZoomWritable Int8 where
    data SummaryWork Int8 = SummaryWorkInt8
        { swInt8Time  :: {-# UNPACK #-}!TimeStamp
        , swInt8Entry :: !(Maybe Int8)
        , swInt8Exit  :: {-# UNPACK #-}!Int8
        , swInt8Min   :: {-# UNPACK #-}!Int8
        , swInt8Max   :: {-# UNPACK #-}!Int8
        , swInt8Sum   :: {-# UNPACK #-}!Double
        , swInt8SumSq :: {-# UNPACK #-}!Double
        }

    fromRaw           = fromInt8
    fromSummaryData   = fromSummaryNum

    initSummaryWork   = initSummaryNumBounded
    toSummaryData     = mkSummaryNum
    updateSummaryData = updateSummaryNum
    appendSummaryData = appendSummaryNum

instance ZoomNum Int8 where
    numEntry = summaryInt8Entry
    numExit = summaryInt8Exit
    numMin = summaryInt8Min
    numMax = summaryInt8Max
    numAvg = summaryInt8Avg
    numRMS = summaryInt8RMS

    numWorkTime = swInt8Time
    numWorkEntry = swInt8Entry
    numWorkExit = swInt8Exit
    numWorkMin = swInt8Min
    numWorkMax = swInt8Max
    numWorkSum = swInt8Sum
    numWorkSumSq = swInt8SumSq

    numMkSummary = SummaryInt8
    numMkSummaryWork = SummaryWorkInt8

{-# SPECIALIZE fromSummaryNum :: SummaryData Int8 -> Builder #-}
{-# SPECIALIZE initSummaryNumBounded :: TimeStamp -> SummaryWork Int8 #-}
{-# SPECIALIZE mkSummaryNum :: TimeStampDiff -> SummaryWork Int8 -> SummaryData Int8 #-}
{-# SPECIALIZE appendSummaryNum :: TimeStampDiff -> SummaryData Int8 -> TimeStampDiff -> SummaryData Int8 -> SummaryData Int8 #-}
{-# SPECIALIZE updateSummaryNum :: TimeStamp -> Int8 -> SummaryWork Int8 -> SummaryWork Int8 #-}

----------------------------------------------------------------------
-- Int16

instance ZoomReadable Int16 where
    data SummaryData Int16 = SummaryInt16
        { summaryInt16Entry :: {-# UNPACK #-}!Int16
        , summaryInt16Exit  :: {-# UNPACK #-}!Int16
        , summaryInt16Min   :: {-# UNPACK #-}!Int16
        , summaryInt16Max   :: {-# UNPACK #-}!Int16
        , summaryInt16Avg   :: {-# UNPACK #-}!Double
        , summaryInt16RMS   :: {-# UNPACK #-}!Double
        }

    trackIdentifier = const "ZOOMi16b"

    readRaw     = readInt16be
    readSummary = readSummaryNum

    prettyRaw         = show
    prettySummaryData = prettySummaryInt

{-# SPECIALIZE readSummaryNum :: (Functor m, MonadIO m) => Iteratee [Word8] m (SummaryData Int16) #-}
{-# SPECIALIZE readSummaryNum :: (Functor m, MonadIO m) => Iteratee ByteString m (SummaryData Int16) #-}

instance ZoomWrite Int16 where
    write = writeData

instance ZoomWrite (TimeStamp, Int16) where
    write = writeDataVBR

instance ZoomWritable Int16 where
    data SummaryWork Int16 = SummaryWorkInt16
        { swInt16Time  :: {-# UNPACK #-}!TimeStamp
        , swInt16Entry :: !(Maybe Int16)
        , swInt16Exit  :: {-# UNPACK #-}!Int16
        , swInt16Min   :: {-# UNPACK #-}!Int16
        , swInt16Max   :: {-# UNPACK #-}!Int16
        , swInt16Sum   :: {-# UNPACK #-}!Double
        , swInt16SumSq :: {-# UNPACK #-}!Double
        }

    fromRaw           = fromInt16be
    fromSummaryData   = fromSummaryNum

    initSummaryWork   = initSummaryNumBounded
    toSummaryData     = mkSummaryNum
    updateSummaryData = updateSummaryNum
    appendSummaryData = appendSummaryNum

instance ZoomNum Int16 where
    numEntry = summaryInt16Entry
    numExit = summaryInt16Exit
    numMin = summaryInt16Min
    numMax = summaryInt16Max
    numAvg = summaryInt16Avg
    numRMS = summaryInt16RMS

    numWorkTime = swInt16Time
    numWorkEntry = swInt16Entry
    numWorkExit = swInt16Exit
    numWorkMin = swInt16Min
    numWorkMax = swInt16Max
    numWorkSum = swInt16Sum
    numWorkSumSq = swInt16SumSq

    numMkSummary = SummaryInt16
    numMkSummaryWork = SummaryWorkInt16

{-# SPECIALIZE fromSummaryNum :: SummaryData Int16 -> Builder #-}
{-# SPECIALIZE initSummaryNumBounded :: TimeStamp -> SummaryWork Int16 #-}
{-# SPECIALIZE mkSummaryNum :: TimeStampDiff -> SummaryWork Int16 -> SummaryData Int16 #-}
{-# SPECIALIZE appendSummaryNum :: TimeStampDiff -> SummaryData Int16 -> TimeStampDiff -> SummaryData Int16 -> SummaryData Int16 #-}
{-# SPECIALIZE updateSummaryNum :: TimeStamp -> Int16 -> SummaryWork Int16 -> SummaryWork Int16 #-}

----------------------------------------------------------------------
-- Int32

instance ZoomReadable Int32 where
    data SummaryData Int32 = SummaryInt32
        { summaryInt32Entry :: {-# UNPACK #-}!Int32
        , summaryInt32Exit  :: {-# UNPACK #-}!Int32
        , summaryInt32Min   :: {-# UNPACK #-}!Int32
        , summaryInt32Max   :: {-# UNPACK #-}!Int32
        , summaryInt32Avg   :: {-# UNPACK #-}!Double
        , summaryInt32RMS   :: {-# UNPACK #-}!Double
        }

    trackIdentifier = const "ZOOMi32b"

    readRaw     = readInt32be
    readSummary = readSummaryNum

    prettyRaw         = show
    prettySummaryData = prettySummaryInt

{-# SPECIALIZE readSummaryNum :: (Functor m, MonadIO m) => Iteratee [Word8] m (SummaryData Int32) #-}
{-# SPECIALIZE readSummaryNum :: (Functor m, MonadIO m) => Iteratee ByteString m (SummaryData Int32) #-}

instance ZoomWrite Int32 where
    write = writeData

instance ZoomWrite (TimeStamp, Int32) where
    write = writeDataVBR

instance ZoomWritable Int32 where
    data SummaryWork Int32 = SummaryWorkInt32
        { swInt32Time  :: {-# UNPACK #-}!TimeStamp
        , swInt32Entry :: !(Maybe Int32)
        , swInt32Exit  :: {-# UNPACK #-}!Int32
        , swInt32Min   :: {-# UNPACK #-}!Int32
        , swInt32Max   :: {-# UNPACK #-}!Int32
        , swInt32Sum   :: {-# UNPACK #-}!Double
        , swInt32SumSq :: {-# UNPACK #-}!Double
        }

    fromRaw           = fromIntegral32be
    fromSummaryData   = fromSummaryNum

    initSummaryWork   = initSummaryNumBounded
    toSummaryData     = mkSummaryNum
    updateSummaryData = updateSummaryNum
    appendSummaryData = appendSummaryNum

instance ZoomNum Int32 where
    numEntry = summaryInt32Entry
    numExit = summaryInt32Exit
    numMin = summaryInt32Min
    numMax = summaryInt32Max
    numAvg = summaryInt32Avg
    numRMS = summaryInt32RMS

    numWorkTime = swInt32Time
    numWorkEntry = swInt32Entry
    numWorkExit = swInt32Exit
    numWorkMin = swInt32Min
    numWorkMax = swInt32Max
    numWorkSum = swInt32Sum
    numWorkSumSq = swInt32SumSq

    numMkSummary = SummaryInt32
    numMkSummaryWork = SummaryWorkInt32

{-# SPECIALIZE fromSummaryNum :: SummaryData Int32 -> Builder #-}
{-# SPECIALIZE initSummaryNumBounded :: TimeStamp -> SummaryWork Int32 #-}
{-# SPECIALIZE mkSummaryNum :: TimeStampDiff -> SummaryWork Int32 -> SummaryData Int32 #-}
{-# SPECIALIZE appendSummaryNum :: TimeStampDiff -> SummaryData Int32 -> TimeStampDiff -> SummaryData Int32 -> SummaryData Int32 #-}
{-# SPECIALIZE updateSummaryNum :: TimeStamp -> Int32 -> SummaryWork Int32 -> SummaryWork Int32 #-}

----------------------------------------------------------------------
-- Int64

instance ZoomReadable Int64 where
    data SummaryData Int64 = SummaryInt64
        { summaryInt64Entry :: {-# UNPACK #-}!Int64
        , summaryInt64Exit  :: {-# UNPACK #-}!Int64
        , summaryInt64Min   :: {-# UNPACK #-}!Int64
        , summaryInt64Max   :: {-# UNPACK #-}!Int64
        , summaryInt64Avg   :: {-# UNPACK #-}!Double
        , summaryInt64RMS   :: {-# UNPACK #-}!Double
        }

    trackIdentifier = const "ZOOMi64b"

    readRaw     = readInt64be
    readSummary = readSummaryNum

    prettyRaw         = show
    prettySummaryData = prettySummaryInt

{-# SPECIALIZE readSummaryNum :: (Functor m, MonadIO m) => Iteratee [Word8] m (SummaryData Int64) #-}
{-# SPECIALIZE readSummaryNum :: (Functor m, MonadIO m) => Iteratee ByteString m (SummaryData Int64) #-}

instance ZoomWrite Int64 where
    write = writeData

instance ZoomWrite (TimeStamp, Int64) where
    write = writeDataVBR

instance ZoomWritable Int64 where
    data SummaryWork Int64 = SummaryWorkInt64
        { swInt64Time  :: {-# UNPACK #-}!TimeStamp
        , swInt64Entry :: !(Maybe Int64)
        , swInt64Exit  :: {-# UNPACK #-}!Int64
        , swInt64Min   :: {-# UNPACK #-}!Int64
        , swInt64Max   :: {-# UNPACK #-}!Int64
        , swInt64Sum   :: {-# UNPACK #-}!Double
        , swInt64SumSq :: {-# UNPACK #-}!Double
        }

    fromRaw           = fromInt64be
    fromSummaryData   = fromSummaryNum

    initSummaryWork   = initSummaryNumBounded
    toSummaryData     = mkSummaryNum
    updateSummaryData = updateSummaryNum
    appendSummaryData = appendSummaryNum

instance ZoomNum Int64 where
    numEntry = summaryInt64Entry
    numExit = summaryInt64Exit
    numMin = summaryInt64Min
    numMax = summaryInt64Max
    numAvg = summaryInt64Avg
    numRMS = summaryInt64RMS

    numWorkTime = swInt64Time
    numWorkEntry = swInt64Entry
    numWorkExit = swInt64Exit
    numWorkMin = swInt64Min
    numWorkMax = swInt64Max
    numWorkSum = swInt64Sum
    numWorkSumSq = swInt64SumSq

    numMkSummary = SummaryInt64
    numMkSummaryWork = SummaryWorkInt64

{-# SPECIALIZE fromSummaryNum :: SummaryData Int64 -> Builder #-}
{-# SPECIALIZE initSummaryNumBounded :: TimeStamp -> SummaryWork Int64 #-}
{-# SPECIALIZE mkSummaryNum :: TimeStampDiff -> SummaryWork Int64 -> SummaryData Int64 #-}
{-# SPECIALIZE appendSummaryNum :: TimeStampDiff -> SummaryData Int64 -> TimeStampDiff -> SummaryData Int64 -> SummaryData Int64 #-}
{-# SPECIALIZE updateSummaryNum :: TimeStamp -> Int64 -> SummaryWork Int64 -> SummaryWork Int64 #-}

----------------------------------------------------------------------

prettySummaryInt :: (PrintfArg a, ZoomNum a)
                 => SummaryData a -> String
prettySummaryInt s = concat
    [ printf "\tentry: %d\texit: %df\tmin: %d\tmax: %d\t"
          (numEntry s) (numExit s) (numMin s) (numMax s)
    , printf "avg: %.3f\trms: %.3f" (numAvg s) (numRMS s)
    ]


{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
{- |
   Module      : Data.ZoomCache.Numeric.Word
   Copyright   : Conrad Parker
   License     : BSD3-style (see LICENSE)

   Maintainer  : Conrad Parker <conrad@metadecks.org>
   Stability   : unstable
   Portability : unknown

Default codec implementation for values of type Word. This module
implements the interfaces documented in "Data.ZoomCache.Codec".
View the module source for enlightenment.

The table below describes the encoding of SummaryData for Word8:

@
   | ...                                                           |   -35
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Entry (word8) | Exit (word8)  | Min (word8)   | Max (word8)   | 36-39
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Avg (double)                                                  | 40-43
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 44-47
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | RMS (double)                                                  | 48-51
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
@

The table below describes the encoding of SummaryData for Word16:

@
   | ...                                                           |   -35
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Entry (word16)                | Exit (word16)                 | 36-39
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Min (word16)                  | Max (word16)                  | 40-43
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

The table below describes the encoding of SummaryData for Word32:

@
   | ...                                                           |   -35
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Entry (word32)                                                | 36-39
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Exit (word32)                                                 | 40-43
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Min (word32)                                                  | 44-47
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Max (word32)                                                  | 48-51
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

The table below describes the encoding of SummaryData for Word64:

@
   | ...                                                           |   -35
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Entry (word64)                                                | 36-39
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 40-43
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Exit (word64)                                                 | 44-47
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 48-51
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Min (word64)                                                  | 52-55
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 56-59
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Max (word64)                                                  | 60-63
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 64-67
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Avg (double)                                                  | 68-71
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 72-75
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | RMS (double)                                                  | 76-79
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 80-83
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
@

SummaryData for Word is encoded as the following sequence, in which the
variable-length coded sign bit is always zero:

@
   Entry (intVLC)
   Exit (intVLC)
   Min (intVLC)
   Max (intVLC)
   Avg (double)
   RMS (double)
@

Field encoding formats:

  @word8@:   8bit unsigned integer

  @word16@:  16bit big endian unsigned integer

  @word32@:  32bit big endian unsigned integer

  @word64@:  32bit big endian unsigned integer

  @intVLC@: Variable-length-coded signed integer

  @double@: big-endian IEEE 754-2008 binary64 (IEEE 754-1985 double)

For details of the variable-length coding format, see "Data.ZoomCache.Numeric.Int".

-}
----------------------------------------------------------------------

module Data.ZoomCache.Numeric.Word (
      SummaryData(..)
    , SummaryWork(..)
)where

#if __GLASGOW_HASKELL__ >= 702
import Data.ByteString (ByteString)
import Data.Iteratee (Iteratee)
#endif

import Blaze.ByteString.Builder
import Control.Applicative ((<$>))
import Data.Word
import Text.Printf

import Data.ZoomCache.Codec
import Data.ZoomCache.Numeric.Internal
import Data.ZoomCache.Numeric.Types

----------------------------------------------------------------------
-- Word

instance ZoomReadable Word where
    data SummaryData Word = SummaryWord
        { summaryWordEntry :: {-# UNPACK #-}!Word
        , summaryWordExit  :: {-# UNPACK #-}!Word
        , summaryWordMin   :: {-# UNPACK #-}!Word
        , summaryWordMax   :: {-# UNPACK #-}!Word
        , summaryWordAvg   :: {-# UNPACK #-}!Double
        , summaryWordRMS   :: {-# UNPACK #-}!Double
        }

    trackIdentifier = const "ZOOMuntb"

    readRaw     = fromIntegral <$> readIntegerVLC
    readSummary = readSummaryNum

    prettyRaw         = show
    prettySummaryData = prettySummaryWord

#if __GLASGOW_HASKELL__ >= 702
{-# SPECIALIZE readSummaryNum :: (Functor m, Monad m) => Iteratee ByteString m (SummaryData Word) #-}
#endif

instance ZoomWrite Word where
    write = writeData

instance ZoomWrite (SampleOffset, Word) where
    write = writeDataVBR

instance ZoomWrite (TimeStamp, Word) where
    write = writeDataTS

instance ZoomWritable Word where
    data SummaryWork Word = SummaryWorkWord
        { swWordTime  :: {-# UNPACK #-}!SampleOffset
        , swWordEntry :: !(Maybe Word)
        , swWordExit  :: {-# UNPACK #-}!Word
        , swWordMin   :: {-# UNPACK #-}!Word
        , swWordMax   :: {-# UNPACK #-}!Word
        , swWordSum   :: {-# UNPACK #-}!Double
        , swWordSumSq :: {-# UNPACK #-}!Double
        }

    fromRaw           = fromIntegerVLC . fromIntegral
    fromSummaryData   = fromSummaryNum

    initSummaryWork   = initSummaryNumBounded
    toSummaryData     = mkSummaryNum
    updateSummaryData = updateSummaryNum
    appendSummaryData = appendSummaryNum

instance ZoomNum Word where
    numEntry = summaryWordEntry
    numExit = summaryWordExit
    numMin = summaryWordMin
    numMax = summaryWordMax
    numAvg = summaryWordAvg
    numRMS = summaryWordRMS

    numWorkSO = swWordTime
    numWorkEntry = swWordEntry
    numWorkExit = swWordExit
    numWorkMin = swWordMin
    numWorkMax = swWordMax
    numWorkSum = swWordSum
    numWorkSumSq = swWordSumSq

    numMkSummary = SummaryWord
    numMkSummaryWork = SummaryWorkWord

#if __GLASGOW_HASKELL__ >= 702
{-# SPECIALIZE fromSummaryNum :: SummaryData Word -> Builder #-}
{-# SPECIALIZE initSummaryNumBounded :: SampleOffset -> SummaryWork Word #-}
{-# SPECIALIZE mkSummaryNum :: SampleOffsetDiff -> SummaryWork Word -> SummaryData Word #-}
{-# SPECIALIZE appendSummaryNum :: SampleOffsetDiff -> SummaryData Word -> SampleOffsetDiff -> SummaryData Word -> SummaryData Word #-}
{-# SPECIALIZE updateSummaryNum :: SampleOffset -> Word -> SummaryWork Word -> SummaryWork Word #-}
#endif

----------------------------------------------------------------------
-- Word8

instance ZoomReadable Word8 where
    data SummaryData Word8 = SummaryWord8
        { summaryWord8Entry :: {-# UNPACK #-}!Word8
        , summaryWord8Exit  :: {-# UNPACK #-}!Word8
        , summaryWord8Min   :: {-# UNPACK #-}!Word8
        , summaryWord8Max   :: {-# UNPACK #-}!Word8
        , summaryWord8Avg   :: {-# UNPACK #-}!Double
        , summaryWord8RMS   :: {-# UNPACK #-}!Double
        }

    trackIdentifier = const "ZOOMu08b"

    readRaw     = readWord8
    readSummary = readSummaryNum

    prettyRaw         = show
    prettySummaryData = prettySummaryWord

#if __GLASGOW_HASKELL__ >= 702
{-# SPECIALIZE readSummaryNum :: (Functor m, Monad m) => Iteratee ByteString m (SummaryData Word8) #-}
#endif

instance ZoomWrite Word8 where
    write = writeData

instance ZoomWrite (SampleOffset, Word8) where
    write = writeDataVBR

instance ZoomWrite (TimeStamp, Word8) where
    write = writeDataTS

instance ZoomWritable Word8 where
    data SummaryWork Word8 = SummaryWorkWord8
        { swWord8Time  :: {-# UNPACK #-}!SampleOffset
        , swWord8Entry :: !(Maybe Word8)
        , swWord8Exit  :: {-# UNPACK #-}!Word8
        , swWord8Min   :: {-# UNPACK #-}!Word8
        , swWord8Max   :: {-# UNPACK #-}!Word8
        , swWord8Sum   :: {-# UNPACK #-}!Double
        , swWord8SumSq :: {-# UNPACK #-}!Double
        }

    fromRaw           = fromWord8
    fromSummaryData   = fromSummaryNum

    initSummaryWork   = initSummaryNumBounded
    toSummaryData     = mkSummaryNum
    updateSummaryData = updateSummaryNum
    appendSummaryData = appendSummaryNum

instance ZoomNum Word8 where
    numEntry = summaryWord8Entry
    numExit = summaryWord8Exit
    numMin = summaryWord8Min
    numMax = summaryWord8Max
    numAvg = summaryWord8Avg
    numRMS = summaryWord8RMS

    numWorkSO = swWord8Time
    numWorkEntry = swWord8Entry
    numWorkExit = swWord8Exit
    numWorkMin = swWord8Min
    numWorkMax = swWord8Max
    numWorkSum = swWord8Sum
    numWorkSumSq = swWord8SumSq

    numMkSummary = SummaryWord8
    numMkSummaryWork = SummaryWorkWord8

#if __GLASGOW_HASKELL__ >= 702
{-# SPECIALIZE fromSummaryNum :: SummaryData Word8 -> Builder #-}
{-# SPECIALIZE initSummaryNumBounded :: SampleOffset -> SummaryWork Word8 #-}
{-# SPECIALIZE mkSummaryNum :: SampleOffsetDiff -> SummaryWork Word8 -> SummaryData Word8 #-}
{-# SPECIALIZE appendSummaryNum :: SampleOffsetDiff -> SummaryData Word8 -> SampleOffsetDiff -> SummaryData Word8 -> SummaryData Word8 #-}
{-# SPECIALIZE updateSummaryNum :: SampleOffset -> Word8 -> SummaryWork Word8 -> SummaryWork Word8 #-}
#endif

----------------------------------------------------------------------
-- Word16

instance ZoomReadable Word16 where
    data SummaryData Word16 = SummaryWord16
        { summaryWord16Entry :: {-# UNPACK #-}!Word16
        , summaryWord16Exit  :: {-# UNPACK #-}!Word16
        , summaryWord16Min   :: {-# UNPACK #-}!Word16
        , summaryWord16Max   :: {-# UNPACK #-}!Word16
        , summaryWord16Avg   :: {-# UNPACK #-}!Double
        , summaryWord16RMS   :: {-# UNPACK #-}!Double
        }

    trackIdentifier = const "ZOOMu16b"

    readRaw     = readWord16be
    readSummary = readSummaryNum

    prettyRaw         = show
    prettySummaryData = prettySummaryWord

#if __GLASGOW_HASKELL__ >= 702
{-# SPECIALIZE readSummaryNum :: (Functor m, Monad m) => Iteratee ByteString m (SummaryData Word16) #-}
#endif

instance ZoomWrite Word16 where
    write = writeData

instance ZoomWrite (SampleOffset, Word16) where
    write = writeDataVBR

instance ZoomWrite (TimeStamp, Word16) where
    write = writeDataTS

instance ZoomWritable Word16 where
    data SummaryWork Word16 = SummaryWorkWord16
        { swWord16Time  :: {-# UNPACK #-}!SampleOffset
        , swWord16Entry :: !(Maybe Word16)
        , swWord16Exit  :: {-# UNPACK #-}!Word16
        , swWord16Min   :: {-# UNPACK #-}!Word16
        , swWord16Max   :: {-# UNPACK #-}!Word16
        , swWord16Sum   :: {-# UNPACK #-}!Double
        , swWord16SumSq :: {-# UNPACK #-}!Double
        }

    fromRaw           = fromWord16be
    fromSummaryData   = fromSummaryNum

    initSummaryWork   = initSummaryNumBounded
    toSummaryData     = mkSummaryNum
    updateSummaryData = updateSummaryNum
    appendSummaryData = appendSummaryNum

instance ZoomNum Word16 where
    numEntry = summaryWord16Entry
    numExit = summaryWord16Exit
    numMin = summaryWord16Min
    numMax = summaryWord16Max
    numAvg = summaryWord16Avg
    numRMS = summaryWord16RMS

    numWorkSO = swWord16Time
    numWorkEntry = swWord16Entry
    numWorkExit = swWord16Exit
    numWorkMin = swWord16Min
    numWorkMax = swWord16Max
    numWorkSum = swWord16Sum
    numWorkSumSq = swWord16SumSq

    numMkSummary = SummaryWord16
    numMkSummaryWork = SummaryWorkWord16

#if __GLASGOW_HASKELL__ >= 702
{-# SPECIALIZE fromSummaryNum :: SummaryData Word16 -> Builder #-}
{-# SPECIALIZE initSummaryNumBounded :: SampleOffset -> SummaryWork Word16 #-}
{-# SPECIALIZE mkSummaryNum :: SampleOffsetDiff -> SummaryWork Word16 -> SummaryData Word16 #-}
{-# SPECIALIZE appendSummaryNum :: SampleOffsetDiff -> SummaryData Word16 -> SampleOffsetDiff -> SummaryData Word16 -> SummaryData Word16 #-}
{-# SPECIALIZE updateSummaryNum :: SampleOffset -> Word16 -> SummaryWork Word16 -> SummaryWork Word16 #-}
#endif

----------------------------------------------------------------------
-- Word32

instance ZoomReadable Word32 where
    data SummaryData Word32 = SummaryWord32
        { summaryWord32Entry :: {-# UNPACK #-}!Word32
        , summaryWord32Exit  :: {-# UNPACK #-}!Word32
        , summaryWord32Min   :: {-# UNPACK #-}!Word32
        , summaryWord32Max   :: {-# UNPACK #-}!Word32
        , summaryWord32Avg   :: {-# UNPACK #-}!Double
        , summaryWord32RMS   :: {-# UNPACK #-}!Double
        }

    trackIdentifier = const "ZOOMu32b"

    readRaw     = readWord32be
    readSummary = readSummaryNum

    prettyRaw         = show
    prettySummaryData = prettySummaryWord

#if __GLASGOW_HASKELL__ >= 702
{-# SPECIALIZE readSummaryNum :: (Functor m, Monad m) => Iteratee ByteString m (SummaryData Word32) #-}
#endif

instance ZoomWrite Word32 where
    write = writeData

instance ZoomWrite (SampleOffset, Word32) where
    write = writeDataVBR

instance ZoomWrite (TimeStamp, Word32) where
    write = writeDataTS

instance ZoomWritable Word32 where
    data SummaryWork Word32 = SummaryWorkWord32
        { swWord32Time  :: {-# UNPACK #-}!SampleOffset
        , swWord32Entry :: !(Maybe Word32)
        , swWord32Exit  :: {-# UNPACK #-}!Word32
        , swWord32Min   :: {-# UNPACK #-}!Word32
        , swWord32Max   :: {-# UNPACK #-}!Word32
        , swWord32Sum   :: {-# UNPACK #-}!Double
        , swWord32SumSq :: {-# UNPACK #-}!Double
        }

    fromRaw           = fromWord32be
    fromSummaryData   = fromSummaryNum

    initSummaryWork   = initSummaryNumBounded
    toSummaryData     = mkSummaryNum
    updateSummaryData = updateSummaryNum
    appendSummaryData = appendSummaryNum

instance ZoomNum Word32 where
    numEntry = summaryWord32Entry
    numExit = summaryWord32Exit
    numMin = summaryWord32Min
    numMax = summaryWord32Max
    numAvg = summaryWord32Avg
    numRMS = summaryWord32RMS

    numWorkSO = swWord32Time
    numWorkEntry = swWord32Entry
    numWorkExit = swWord32Exit
    numWorkMin = swWord32Min
    numWorkMax = swWord32Max
    numWorkSum = swWord32Sum
    numWorkSumSq = swWord32SumSq

    numMkSummary = SummaryWord32
    numMkSummaryWork = SummaryWorkWord32

#if __GLASGOW_HASKELL__ >= 702
{-# SPECIALIZE fromSummaryNum :: SummaryData Word32 -> Builder #-}
{-# SPECIALIZE initSummaryNumBounded :: SampleOffset -> SummaryWork Word32 #-}
{-# SPECIALIZE mkSummaryNum :: SampleOffsetDiff -> SummaryWork Word32 -> SummaryData Word32 #-}
{-# SPECIALIZE appendSummaryNum :: SampleOffsetDiff -> SummaryData Word32 -> SampleOffsetDiff -> SummaryData Word32 -> SummaryData Word32 #-}
{-# SPECIALIZE updateSummaryNum :: SampleOffset -> Word32 -> SummaryWork Word32 -> SummaryWork Word32 #-}
#endif

----------------------------------------------------------------------
-- Word64

instance ZoomReadable Word64 where
    data SummaryData Word64 = SummaryWord64
        { summaryWord64Entry :: {-# UNPACK #-}!Word64
        , summaryWord64Exit  :: {-# UNPACK #-}!Word64
        , summaryWord64Min   :: {-# UNPACK #-}!Word64
        , summaryWord64Max   :: {-# UNPACK #-}!Word64
        , summaryWord64Avg   :: {-# UNPACK #-}!Double
        , summaryWord64RMS   :: {-# UNPACK #-}!Double
        }

    trackIdentifier = const "ZOOMu64b"

    readRaw     = readWord64be
    readSummary = readSummaryNum

    prettyRaw         = show
    prettySummaryData = prettySummaryWord

#if __GLASGOW_HASKELL__ >= 702
{-# SPECIALIZE readSummaryNum :: (Functor m, Monad m) => Iteratee ByteString m (SummaryData Word64) #-}
#endif

instance ZoomWrite Word64 where
    write = writeData

instance ZoomWrite (SampleOffset, Word64) where
    write = writeDataVBR

instance ZoomWrite (TimeStamp, Word64) where
    write = writeDataTS

instance ZoomWritable Word64 where
    data SummaryWork Word64 = SummaryWorkWord64
        { swWord64Time  :: {-# UNPACK #-}!SampleOffset
        , swWord64Entry :: !(Maybe Word64)
        , swWord64Exit  :: {-# UNPACK #-}!Word64
        , swWord64Min   :: {-# UNPACK #-}!Word64
        , swWord64Max   :: {-# UNPACK #-}!Word64
        , swWord64Sum   :: {-# UNPACK #-}!Double
        , swWord64SumSq :: {-# UNPACK #-}!Double
        }

    fromRaw           = fromWord64be
    fromSummaryData   = fromSummaryNum

    initSummaryWork   = initSummaryNumBounded
    toSummaryData     = mkSummaryNum
    updateSummaryData = updateSummaryNum
    appendSummaryData = appendSummaryNum

instance ZoomNum Word64 where
    numEntry = summaryWord64Entry
    numExit = summaryWord64Exit
    numMin = summaryWord64Min
    numMax = summaryWord64Max
    numAvg = summaryWord64Avg
    numRMS = summaryWord64RMS

    numWorkSO = swWord64Time
    numWorkEntry = swWord64Entry
    numWorkExit = swWord64Exit
    numWorkMin = swWord64Min
    numWorkMax = swWord64Max
    numWorkSum = swWord64Sum
    numWorkSumSq = swWord64SumSq

    numMkSummary = SummaryWord64
    numMkSummaryWork = SummaryWorkWord64

#if __GLASGOW_HASKELL__ >= 702
{-# SPECIALIZE fromSummaryNum :: SummaryData Word64 -> Builder #-}
{-# SPECIALIZE initSummaryNumBounded :: SampleOffset -> SummaryWork Word64 #-}
{-# SPECIALIZE mkSummaryNum :: SampleOffsetDiff -> SummaryWork Word64 -> SummaryData Word64 #-}
{-# SPECIALIZE appendSummaryNum :: SampleOffsetDiff -> SummaryData Word64 -> SampleOffsetDiff -> SummaryData Word64 -> SummaryData Word64 #-}
{-# SPECIALIZE updateSummaryNum :: SampleOffset -> Word64 -> SummaryWork Word64 -> SummaryWork Word64 #-}
#endif

----------------------------------------------------------------------

prettySummaryWord :: (PrintfArg a, ZoomNum a)
                 => SummaryData a -> String
prettySummaryWord s = concat
    [ printf "\tentry: %d\texit: %df\tmin: %d\tmax: %d\t"
          (numEntry s) (numExit s) (numMin s) (numMax s)
    , printf "avg: %.3f\trms: %.3f" (numAvg s) (numRMS s)
    ]


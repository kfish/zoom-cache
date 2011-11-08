{-# LANGUAGE BangPatterns #-}
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

The table below describes the encoding of SummaryData for Word and Word32:

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

Field encoding formats:

  @word8@:   8bit unsigned integer

  @word16@:  16bit big endian unsigned integer

  @word32@:  32bit big endian unsigned integer

  @word64@:  32bit big endian unsigned integer

  @double@: big-endian IEEE 754-2008 binary64 (IEEE 754-1985 double)

-}
----------------------------------------------------------------------

module Data.ZoomCache.Numeric.Word (
      SummaryData(..)
    , SummaryWork(..)
)where

import Blaze.ByteString.Builder
import Control.Monad.Trans (MonadIO)
import Data.ByteString (ByteString)
import Data.Iteratee (Iteratee)
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

    trackIdentifier = const "ZOOMi32b"

    readRaw     = readInt32be
    readSummary = readSummaryNum

    prettyRaw         = show
    prettySummaryData = prettySummaryWord

{-# SPECIALIZE readSummaryNum :: (Functor m, MonadIO m) => Iteratee [Word8] m (SummaryData Word) #-}
{-# SPECIALIZE readSummaryNum :: (Functor m, MonadIO m) => Iteratee ByteString m (SummaryData Word) #-}

instance ZoomWrite Word where
    write = writeData

instance ZoomWrite (TimeStamp, Word) where
    write = writeDataVBR

instance ZoomWritable Word where
    data SummaryWork Word = SummaryWorkWord
        { swWordTime  :: {-# UNPACK #-}!TimeStamp
        , swWordEntry :: !(Maybe Word)
        , swWordExit  :: {-# UNPACK #-}!Word
        , swWordMin   :: {-# UNPACK #-}!Word
        , swWordMax   :: {-# UNPACK #-}!Word
        , swWordSum   :: {-# UNPACK #-}!Double
        , swWordSumSq :: {-# UNPACK #-}!Double
        }

    fromRaw           = fromIntegral32be
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

    numWorkTime = swWordTime
    numWorkEntry = swWordEntry
    numWorkExit = swWordExit
    numWorkMin = swWordMin
    numWorkMax = swWordMax
    numWorkSum = swWordSum
    numWorkSumSq = swWordSumSq

    numMkSummary = SummaryWord
    numMkSummaryWork = SummaryWorkWord

{-# SPECIALIZE fromSummaryNum :: SummaryData Word -> Builder #-}
{-# SPECIALIZE initSummaryNumBounded :: TimeStamp -> SummaryWork Word #-}
{-# SPECIALIZE mkSummaryNum :: TimeStampDiff -> SummaryWork Word -> SummaryData Word #-}
{-# SPECIALIZE appendSummaryNum :: TimeStampDiff -> SummaryData Word -> TimeStampDiff -> SummaryData Word -> SummaryData Word #-}
{-# SPECIALIZE updateSummaryNum :: TimeStamp -> Word -> SummaryWork Word -> SummaryWork Word #-}

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

    trackIdentifier = const "ZOOMiS8b"

    readRaw     = readInt8
    readSummary = readSummaryNum

    prettyRaw         = show
    prettySummaryData = prettySummaryWord

{-# SPECIALIZE readSummaryNum :: (Functor m, MonadIO m) => Iteratee [Word8] m (SummaryData Word8) #-}
{-# SPECIALIZE readSummaryNum :: (Functor m, MonadIO m) => Iteratee ByteString m (SummaryData Word8) #-}

instance ZoomWrite Word8 where
    write = writeData

instance ZoomWrite (TimeStamp, Word8) where
    write = writeDataVBR

instance ZoomWritable Word8 where
    data SummaryWork Word8 = SummaryWorkWord8
        { swWord8Time  :: {-# UNPACK #-}!TimeStamp
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

    numWorkTime = swWord8Time
    numWorkEntry = swWord8Entry
    numWorkExit = swWord8Exit
    numWorkMin = swWord8Min
    numWorkMax = swWord8Max
    numWorkSum = swWord8Sum
    numWorkSumSq = swWord8SumSq

    numMkSummary = SummaryWord8
    numMkSummaryWork = SummaryWorkWord8

{-# SPECIALIZE fromSummaryNum :: SummaryData Word8 -> Builder #-}
{-# SPECIALIZE initSummaryNumBounded :: TimeStamp -> SummaryWork Word8 #-}
{-# SPECIALIZE mkSummaryNum :: TimeStampDiff -> SummaryWork Word8 -> SummaryData Word8 #-}
{-# SPECIALIZE appendSummaryNum :: TimeStampDiff -> SummaryData Word8 -> TimeStampDiff -> SummaryData Word8 -> SummaryData Word8 #-}
{-# SPECIALIZE updateSummaryNum :: TimeStamp -> Word8 -> SummaryWork Word8 -> SummaryWork Word8 #-}

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

    trackIdentifier = const "ZOOMi16b"

    readRaw     = readInt16be
    readSummary = readSummaryNum

    prettyRaw         = show
    prettySummaryData = prettySummaryWord

{-# SPECIALIZE readSummaryNum :: (Functor m, MonadIO m) => Iteratee [Word8] m (SummaryData Word16) #-}
{-# SPECIALIZE readSummaryNum :: (Functor m, MonadIO m) => Iteratee ByteString m (SummaryData Word16) #-}

instance ZoomWrite Word16 where
    write = writeData

instance ZoomWrite (TimeStamp, Word16) where
    write = writeDataVBR

instance ZoomWritable Word16 where
    data SummaryWork Word16 = SummaryWorkWord16
        { swWord16Time  :: {-# UNPACK #-}!TimeStamp
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

    numWorkTime = swWord16Time
    numWorkEntry = swWord16Entry
    numWorkExit = swWord16Exit
    numWorkMin = swWord16Min
    numWorkMax = swWord16Max
    numWorkSum = swWord16Sum
    numWorkSumSq = swWord16SumSq

    numMkSummary = SummaryWord16
    numMkSummaryWork = SummaryWorkWord16

{-# SPECIALIZE fromSummaryNum :: SummaryData Word16 -> Builder #-}
{-# SPECIALIZE initSummaryNumBounded :: TimeStamp -> SummaryWork Word16 #-}
{-# SPECIALIZE mkSummaryNum :: TimeStampDiff -> SummaryWork Word16 -> SummaryData Word16 #-}
{-# SPECIALIZE appendSummaryNum :: TimeStampDiff -> SummaryData Word16 -> TimeStampDiff -> SummaryData Word16 -> SummaryData Word16 #-}
{-# SPECIALIZE updateSummaryNum :: TimeStamp -> Word16 -> SummaryWork Word16 -> SummaryWork Word16 #-}

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

    trackIdentifier = const "ZOOMi32b"

    readRaw     = readInt32be
    readSummary = readSummaryNum

    prettyRaw         = show
    prettySummaryData = prettySummaryWord

{-# SPECIALIZE readSummaryNum :: (Functor m, MonadIO m) => Iteratee [Word8] m (SummaryData Word32) #-}
{-# SPECIALIZE readSummaryNum :: (Functor m, MonadIO m) => Iteratee ByteString m (SummaryData Word32) #-}

instance ZoomWrite Word32 where
    write = writeData

instance ZoomWrite (TimeStamp, Word32) where
    write = writeDataVBR

instance ZoomWritable Word32 where
    data SummaryWork Word32 = SummaryWorkWord32
        { swWord32Time  :: {-# UNPACK #-}!TimeStamp
        , swWord32Entry :: !(Maybe Word32)
        , swWord32Exit  :: {-# UNPACK #-}!Word32
        , swWord32Min   :: {-# UNPACK #-}!Word32
        , swWord32Max   :: {-# UNPACK #-}!Word32
        , swWord32Sum   :: {-# UNPACK #-}!Double
        , swWord32SumSq :: {-# UNPACK #-}!Double
        }

    fromRaw           = fromIntegral32be
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

    numWorkTime = swWord32Time
    numWorkEntry = swWord32Entry
    numWorkExit = swWord32Exit
    numWorkMin = swWord32Min
    numWorkMax = swWord32Max
    numWorkSum = swWord32Sum
    numWorkSumSq = swWord32SumSq

    numMkSummary = SummaryWord32
    numMkSummaryWork = SummaryWorkWord32

{-# SPECIALIZE fromSummaryNum :: SummaryData Word32 -> Builder #-}
{-# SPECIALIZE initSummaryNumBounded :: TimeStamp -> SummaryWork Word32 #-}
{-# SPECIALIZE mkSummaryNum :: TimeStampDiff -> SummaryWork Word32 -> SummaryData Word32 #-}
{-# SPECIALIZE appendSummaryNum :: TimeStampDiff -> SummaryData Word32 -> TimeStampDiff -> SummaryData Word32 -> SummaryData Word32 #-}
{-# SPECIALIZE updateSummaryNum :: TimeStamp -> Word32 -> SummaryWork Word32 -> SummaryWork Word32 #-}

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

    trackIdentifier = const "ZOOMi64b"

    readRaw     = readInt64be
    readSummary = readSummaryNum

    prettyRaw         = show
    prettySummaryData = prettySummaryWord

{-# SPECIALIZE readSummaryNum :: (Functor m, MonadIO m) => Iteratee [Word8] m (SummaryData Word64) #-}
{-# SPECIALIZE readSummaryNum :: (Functor m, MonadIO m) => Iteratee ByteString m (SummaryData Word64) #-}

instance ZoomWrite Word64 where
    write = writeData

instance ZoomWrite (TimeStamp, Word64) where
    write = writeDataVBR

instance ZoomWritable Word64 where
    data SummaryWork Word64 = SummaryWorkWord64
        { swWord64Time  :: {-# UNPACK #-}!TimeStamp
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

    numWorkTime = swWord64Time
    numWorkEntry = swWord64Entry
    numWorkExit = swWord64Exit
    numWorkMin = swWord64Min
    numWorkMax = swWord64Max
    numWorkSum = swWord64Sum
    numWorkSumSq = swWord64SumSq

    numMkSummary = SummaryWord64
    numMkSummaryWork = SummaryWorkWord64

{-# SPECIALIZE fromSummaryNum :: SummaryData Word64 -> Builder #-}
{-# SPECIALIZE initSummaryNumBounded :: TimeStamp -> SummaryWork Word64 #-}
{-# SPECIALIZE mkSummaryNum :: TimeStampDiff -> SummaryWork Word64 -> SummaryData Word64 #-}
{-# SPECIALIZE appendSummaryNum :: TimeStampDiff -> SummaryData Word64 -> TimeStampDiff -> SummaryData Word64 -> SummaryData Word64 #-}
{-# SPECIALIZE updateSummaryNum :: TimeStamp -> Word64 -> SummaryWork Word64 -> SummaryWork Word64 #-}

----------------------------------------------------------------------

prettySummaryWord :: (PrintfArg a, ZoomNum a)
                 => SummaryData a -> String
prettySummaryWord s = concat
    [ printf "\tentry: %d\texit: %df\tmin: %d\tmax: %d\t"
          (numEntry s) (numExit s) (numMin s) (numMax s)
    , printf "avg: %.3f\trms: %.3f" (numAvg s) (numRMS s)
    ]


{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
{- |
   Module      : Data.ZoomCache.Format
   Copyright   : Conrad Parker
   License     : BSD3-style (see LICENSE)
  
   Maintainer  : Conrad Parker <conrad@metadecks.org>
   Stability   : unstable
   Portability : unknown
  
zoom-cache format specification
  
Field encoding formats:

  @int16@:  16bit big endian

  @int32@:  32bit big endian

  @double@: big-endian IEEE 754-2008 binary64 (IEEE 754-1985 double)
-}
----------------------------------------------------------------------

module Data.ZoomCache.Format (
    -- * One-byte marker at start of all headers
      headerMarker

    -- * Global header
    , globalHeader
    , versionMajor
    , versionMinor

    -- * Track header
    , trackHeader

    -- * Packet header
    , packetHeader

    -- * Summary header
    , summaryHeader
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Word

headerMarker :: Word8
headerMarker = 0xe5

{- |

Global header:

@
    0                   1                   2                   3
    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1| Byte
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Identifier                                                    | 0-3
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | ...                                                           | 4-7
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Version major (int16)         | Version minor (int16)         | 8-11
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | No. tracks (int32)                                            | 12-15
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Modified Julian Day (intVLC)                                  | 16-
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | ...                                                           | ...
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Seconds since midnight Numerator (intVLC)                     | ...
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | ...                                                           | ...
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Seconds since midnight Denominator (intVLC)                   | ...
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | ...                                                           | ...
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
@

The Universal Time corresponding to data timestamp 0 is uniquely given
by the fields "Modified Julian Day", "Seconds since midnight Numerator"
and "Seconds since midnight Denominator".

The Modified Julian Day is a standard count of days, with zero being
the day 1858-11-17.
Seconds since midnight: 0 <= t <= 86401s (because of leap seconds)

If the correspondence to Universal Time is meaningless or unknown when
writing data, these fields shall be given the special values 0, 0, 0.

When reading, a value of 0 for "Seconds since midnight Denominator"
should be interpreted as no corresponding Universal Time.

Field encoding formats:

  @int16@:  16bit big endian signed integer

  @int32@:  32bit big endian signed integer

  @intVLC@: Variable-length-coded signed integer

-}

-- Magic identifier at the beginning of a zoom-cache file.
globalHeader :: ByteString
globalHeader = C.pack "\xe5ZXhe4d\0"

-- | The major version encoded by this library
versionMajor :: Int
versionMajor = 3

-- | The minor version encoded by this library
versionMinor :: Int
versionMinor = 0

{- |

Track header:

@
    0                   1                   2                   3
    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1| Byte
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Identifier                                                    | 0-3
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | ...                                                           | 4-7
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Track no. (int32)                                             | 8-11
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Reserved                                                |z|d|v| 12-15
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Datarate numerator (int64)                                    | 16-19
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | ...                                                           | 20-23
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Datarate denominator (int64)                                  | 24-27
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | ...                                                           | 28-31
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Length of codec identifier in bytes (int32)                   | 32-35
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Codec identifier                                              | 36-39
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | ...                                                           | 40-
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Length of name in bytes (int32)                               | ...
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Name (UTF-8) ...                                              | ...
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
@

  @v@ : Variable data rate flag. 0=CBR, 1=VBR
  @d@ : Delta encode flag. 0=Raw values 1=delta encoded values
  @z@ : Zlib encode flag. 0=uncompressed 1=zlib compressed

Datarate: numerator 0 indicates variable bitrate (all data values are timestamped)
-}

-- Identifier for track headers
trackHeader :: ByteString
trackHeader = C.pack "\xe5ZXtRcK\0"

{- |

Raw Data Packet header:

@
    0                   1                   2                   3
    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1| Byte
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Identifier                                                    | 0-3
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | ...                                                           | 4-7
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Track no. (int32)                                             | 8-11
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Entry Timestamp (int64)                                       | 12-15
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | ...                                                           | 16-19
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Exit TImestamp (int64)                                        | 20-23
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | ...                                                           | 24-27
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Count of data points COUNT (int32)                            | 28-31
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Payload length in bytes (remainder of packet) (int32)         | 32-35
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Data ...                                                      | 36-39
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | ...                                                           | 40-
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Timestamps ...                                                | TS-
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | ...                                                           |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
@

Timestamps block is only present if VBR (datarate numerator is 0)

TS = 28 + (COUNT * sizeof(Type))
-}
-- Identifier for packet headers
packetHeader :: ByteString
packetHeader = C.pack "\xe5ZXp4ck\0"


{- |

Summary Data Packet header:

@
    0                   1                   2                   3
    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1| Byte
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Identifier                                                    | 0-3
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | ...                                                           | 4-7
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Track no. (int32)                                             | 8-11
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Level (int32)                                                 | 12-15
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Entry Timestamp (int64)                                       | 16-19
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | ...                                                           | 20-23
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Exit Timestamp (int64)                                        | 24-27
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | ...                                                           | 28-31
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Summary length in bytes (int32)                               | 32-35
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Summary Data ...                                              | 36-
@

Some default encodings of Summary Data are provided in modules
"Data.ZoomCache.Numeric.Double" and "Data.ZoomCache.Numeric.Int".

-}

-- Identifier for summary headers
summaryHeader :: ByteString
summaryHeader = C.pack "\xe5ZX5umm\0"


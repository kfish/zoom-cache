{-# OPTIONS -Wall #-}

module Data.ByteString.Offset (
    -- * Types
      ByteString(..)
) where

import qualified Data.ByteString as BS
import Data.Nullable
import Data.NullPoint
import System.Posix.Types (FileOffset)

----------------------------------------------------------------------

data ByteString = OBS {-# UNPACK #-}!FileOffset {-# UNPACK #-}!BS.ByteString

instance Nullable ByteString where
    nullC (OBS _ bs) = nullC bs

instance NullPoint ByteString where
    empty = OBS 0 empty

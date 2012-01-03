{-# OPTIONS -Wall #-}

module Data.ByteString.Offset (
    -- * Types
      ByteString(..)
) where

import qualified Data.ByteString as BS
import Data.NullPoint
import System.Posix.Types (FileOffset)

----------------------------------------------------------------------

data ByteString = OBS {-# UNPACK #-}!FileOffset {-# UNPACK #-}!BS.ByteString

instance NullPoint ByteString where
    empty = OBS 0 empty

{-# OPTIONS -Wall #-}

module Data.ByteString.Offset (
    -- * Types
      ByteString(..)
) where

import qualified Data.ByteString as BS
import Data.Int

----------------------------------------------------------------------

data ByteString = OBS {-# UNPACK #-}!Int64 {-# UNPACK #-}!BS.ByteString

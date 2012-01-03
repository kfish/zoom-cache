{-# OPTIONS -Wall #-}

module Data.Offset (
    -- * Types
      Offset(..)
) where

import Data.Nullable
import Data.NullPoint
import System.Posix.Types (FileOffset)

----------------------------------------------------------------------

data Offset a = Offset {-# UNPACK #-}!FileOffset !a

instance Nullable a => Nullable (Offset a) where
    nullC (Offset _ bs) = nullC bs

instance NullPoint a => NullPoint (Offset a) where
    empty = Offset 0 empty

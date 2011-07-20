{-# OPTIONS -Wall #-}

module Zoom.Common (
  zoomHeader
) where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC

------------------------------------------------------------

zoomHeader :: L.ByteString
zoomHeader = LC.pack "ZXe4"

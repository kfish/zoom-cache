{-# LANGUAGE RankNTypes #-}
{-# OPTIONS -Wall #-}

module Data.ZoomCache.Binary (
      buildInt16
    , buildInt32
    , buildInt64
    , encInt
    , encDbl
    , toWord64
) where

import Blaze.ByteString.Builder hiding (flush)
import qualified Data.ByteString.Lazy as L
import Data.Word
import Unsafe.Coerce (unsafeCoerce)

----------------------------------------------------------------------
-- Binary data helpers
    
buildInt16 :: Int -> L.ByteString
buildInt16 = toLazyByteString . fromInt16be . fromIntegral

buildInt32 :: Int -> L.ByteString
buildInt32 = toLazyByteString . fromInt32be . fromIntegral

buildInt64 :: Integer -> L.ByteString
buildInt64 = toLazyByteString . fromInt64be . fromIntegral

encInt :: forall a . (Integral a) => a -> L.ByteString
encInt = toLazyByteString . fromInt32be . fromIntegral

encDbl :: Double -> L.ByteString
encDbl = toLazyByteString . fromWord64be . toWord64

toWord64 :: Double -> Word64
toWord64 = unsafeCoerce


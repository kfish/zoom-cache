{-# OPTIONS -Wall #-}

module Main (main, roundTrip) where

import Blaze.ByteString.Builder
import Data.Functor.Identity
import Data.Int
import qualified Data.Iteratee as I
import Data.TypeLevel.Num hiding ((==))
import Data.Word
import Data.ZoomCache
import Data.ZoomCache.Codec
import Data.ZoomCache.NList

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

----------------------------------------------------------------------
-- * Read, Write roundtrips

runner1 :: Identity (I.Iteratee s Identity c) -> c
runner1 = runIdentity . I.run . runIdentity

-- | Roundtrip
roundTrip :: (Eq a, ZoomReadable a, ZoomWritable a) => a -> Bool
roundTrip x = x == i
    where
        i = runner1 $ I.enumPure1Chunk bs readRaw
        bs = toByteString (fromRaw x)

roundTripUnit :: () -> Bool
roundTripUnit = roundTrip

roundTripBool :: Bool -> Bool
roundTripBool = roundTrip

roundTripInt :: Int -> Bool
roundTripInt = roundTrip

roundTripInt8 :: Int8 -> Bool
roundTripInt8 = roundTrip

roundTripInt16 :: Int16 -> Bool
roundTripInt16 = roundTrip

roundTripInt32 :: Int32 -> Bool
roundTripInt32 = roundTrip

roundTripInt64 :: Int64 -> Bool
roundTripInt64 = roundTrip

roundTripWord :: Word -> Bool
roundTripWord = roundTrip

roundTripWord8 :: Word8 -> Bool
roundTripWord8 = roundTrip

roundTripWord16 :: Word16 -> Bool
roundTripWord16 = roundTrip

roundTripWord32 :: Word32 -> Bool
roundTripWord32 = roundTrip

roundTripWord64 :: Word64 -> Bool
roundTripWord64 = roundTrip

roundTripInteger :: Integer -> Bool
roundTripInteger = roundTrip

roundTripFloat :: Float -> Bool
roundTripFloat = roundTrip

roundTripDouble :: Double -> Bool
roundTripDouble = roundTrip

roundTripNListD7Double :: NList D7 Double -> Bool
roundTripNListD7Double = roundTrip

----------------------------------------------------------------------
-- * Delta encoding roundtrips

deltaEncDec :: (Eq a, Num a) => [a] -> Bool
deltaEncDec xs = xs == encDec
    where
        encDec = deltaDecode (deltaEncode xs)

deltaEncDecInt :: [Int] -> Bool
deltaEncDecInt = deltaEncDec

deltaEncDecInt8 :: [Int8] -> Bool
deltaEncDecInt8 = deltaEncDec

deltaEncDecInt16 :: [Int16] -> Bool
deltaEncDecInt16 = deltaEncDec

deltaEncDecInt32 :: [Int32] -> Bool
deltaEncDecInt32 = deltaEncDec

deltaEncDecInt64 :: [Int64] -> Bool
deltaEncDecInt64 = deltaEncDec

deltaEncDecInteger :: [Integer] -> Bool
deltaEncDecInteger = deltaEncDec

----------------------------------------------------------------------
-- Test harness

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "roundTrip"
      [ testProperty "()" roundTripUnit
      , testProperty "Bool" roundTripBool
      , testProperty "Int" roundTripInt
      , testProperty "Int8" roundTripInt8
      , testProperty "Int16" roundTripInt16
      , testProperty "Int32" roundTripInt32
      , testProperty "Int64" roundTripInt64
      , testProperty "Word" roundTripWord
      , testProperty "Word8" roundTripWord8
      , testProperty "Word16" roundTripWord16
      , testProperty "Word32" roundTripWord32
      , testProperty "Word64" roundTripWord64
      , testProperty "Integer" roundTripInteger
      , testProperty "Float" roundTripFloat
      , testProperty "Double" roundTripDouble
      , testProperty "NList D7 Double" roundTripNListD7Double
      ]
    , testGroup "Delta Encoding"
      [ testProperty "Int" deltaEncDecInt
      , testProperty "Int8" deltaEncDecInt8
      , testProperty "Int16" deltaEncDecInt16
      , testProperty "Int32" deltaEncDecInt32
      , testProperty "Int64" deltaEncDecInt64
      , testProperty "Integer" deltaEncDecInteger
      ]
    ]

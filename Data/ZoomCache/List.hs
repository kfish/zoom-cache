{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
{- |
   Module      : Data.ZoomCache.List
   Copyright   : Conrad Parker
   License     : BSD3-style (see LICENSE)

   Maintainer  : Conrad Parker <conrad@metadecks.org>
   Stability   : unstable
   Portability : unknown

Default codec implementation for multichannel values of type [a].

This module implements the interfaces documented in "Data.ZoomCache.Codec".

The table below describes the encoding of SummaryData for List.

@
   | ...                                                           |   -35
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Expected value (double)                                       | 36-39
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | ...                                                           | 40-43
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
@

Field encoding formats:

  @double@: big-endian IEEE 754-2008 binary64 (IEEE 754-1985 double)

-}
----------------------------------------------------------------------

module Data.ZoomCache.List (
      SummaryData(..)
    , SummaryWork(..)
    , NList(..)
    , listToNList
    , nListToList
)where

import Blaze.ByteString.Builder
import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import Data.Iteratee (Iteratee)
import Data.Monoid (mconcat)
import Data.Typeable
import Data.TypeLevel.Num hiding ((==))
import Test.QuickCheck.Arbitrary
import Unsafe.Coerce

import Data.ZoomCache.Codec

----------------------------------------------------------------------

-- Identifier for track headers
trackTypeNList :: ByteString
trackTypeNList = "ZOOMmchn"

----------------------------------------------------------------------

-- I'm going to hell for this one too
reifyTypeableIntegral :: Integral i => i -> (forall n . (Nat n, Typeable n) => n -> r) -> r
reifyTypeableIntegral i = reifyIntegral i . unsafeCoerce

listToNList :: (Nat n, Typeable n) => [a] -> NList n a
listToNList xs = reifyIntegral (length xs) (\_ -> NList undefined xs)

instance (ZoomWrite a, ZoomWritable a) => ZoomWrite [a] where
    write = writeList

writeList :: (ZoomWrite a, ZoomWritable a) => TrackNo -> [a] -> ZoomW ()
writeList tn xs = reifyTypeableIntegral (length xs) (\n -> write tn (NList n xs))

instance (ZoomWrite a, ZoomWritable a) => ZoomWrite (SampleOffset, [a]) where
    write = writeSOList

writeSOList :: (ZoomWrite a, ZoomWritable a) => TrackNo -> (SampleOffset, [a]) -> ZoomW ()
writeSOList tn (ts, xs) = reifyTypeableIntegral (length xs) (\n -> write tn (ts, (NList n xs)))

----------------------------------------------------------------------

data NList n a = NList n [a]
    deriving (Show, Typeable)

instance Eq a => Eq (NList n a) where
    (NList _ a1) == (NList _ a2) = a1 == a2

instance (Nat n, Arbitrary a) => Arbitrary (NList n a) where
    arbitrary = NList unify <$> sequence [ arbitrary | _ <- [1..(toInt unify)] ]
        where
            unify = undefined

nListToList :: NList n a -> [a]
nListToList (NList _ xs) = xs

----------------------------------------------------------------------
-- Read

instance (Nat n, Typeable n, ZoomReadable a) => ZoomReadable (NList n a) where
    data SummaryData (NList n a) = SummaryNList (NList n (SummaryData a))

    trackIdentifier = mkTrackTypeNList

    readRaw     = readNList
    readSummary = readSummaryNList

    prettyRaw         = prettyPacketNList
    prettySummaryData = prettySummaryNList

mkTrackTypeNList :: (Nat n, ZoomReadable a) => (NList n a) -> ByteString
mkTrackTypeNList (NList nv l) = mconcat
    [ trackTypeNList
    , toByteString . fromIntegral32be . toInt $ nv
    , trackIdentifier (head l)
    ]

prettyPacketNList :: (Nat n, ZoomReadable a) => NList n a -> String
prettyPacketNList (NList _ l) = show (map prettyRaw l)

readNList :: (Functor m, Monad m, Nat n, ZoomReadable a)
          => Iteratee ByteString m (NList n a)
readNList = NList unify <$> replicateM (toInt unify) readRaw
    where
        unify = undefined

readSummaryNList :: (Functor m, Monad m, Nat n, ZoomReadable a)
                 => Iteratee ByteString m (SummaryData (NList n a))
readSummaryNList = SummaryNList .
    NList unify <$> replicateM (toInt unify) readSummary
    where
        unify = undefined

prettySummaryNList :: (Nat n, ZoomReadable a) => SummaryData (NList n a) -> String
prettySummaryNList (SummaryNList (NList _ l)) = show (map prettySummaryData l)

----------------------------------------------------------------------
-- Write

instance (Nat n, Typeable n, ZoomWrite a, ZoomWritable a) => ZoomWrite (NList n a) where
    write = writeData

instance (Nat n, Typeable n, ZoomWrite a, ZoomWritable a) => ZoomWrite (SampleOffset, (NList n a)) where
    write = writeDataVBR

instance (Nat n, NatI n, Typeable n, ZoomWritable a) => ZoomWritable (NList n a) where
    data SummaryWork (NList n a) = SummaryWorkNList (NList n (SummaryWork a))

    fromRaw           = fromNList
    fromSummaryData   = fromSummaryNList

    initSummaryWork   = initSummaryNList
    toSummaryData     = mkSummaryNList
    updateSummaryData = updateSummaryNList
    appendSummaryData = appendSummaryNList

fromNList :: ZoomWritable a => (NList n a) -> Builder
fromNList (NList _ l) = mconcat $ map fromRaw l

initSummaryNList :: (Nat n, ZoomWritable a)
                 => SampleOffset -> SummaryWork (NList n a)
initSummaryNList entry = SummaryWorkNList $
    NList unify (replicate (toInt unify) (initSummaryWork entry))
    where
        unify = undefined

mkSummaryNList :: ZoomWritable a
               => SampleOffsetDiff -> SummaryWork (NList n a) -> SummaryData (NList n a)
mkSummaryNList dur (SummaryWorkNList (NList _ l)) =
    SummaryNList (NList undefined (map (toSummaryData dur) l))

fromSummaryNList :: ZoomWritable a
                 => SummaryData (NList n a) -> Builder
fromSummaryNList (SummaryNList (NList _ l)) = mconcat $ map fromSummaryData l

updateSummaryNList :: ZoomWritable a
                   => SampleOffset  -> (NList n a) -> SummaryWork (NList n a)
                   -> SummaryWork (NList n a)
updateSummaryNList t (NList _ xs) (SummaryWorkNList (NList _ ws)) =
    SummaryWorkNList (NList undefined (zipWith (updateSummaryData t) xs ws))

appendSummaryNList :: ZoomWritable a
                   => SampleOffsetDiff -> SummaryData (NList n a)
                   -> SampleOffsetDiff -> SummaryData (NList n a)
                   -> SummaryData (NList n a)
appendSummaryNList dur1 (SummaryNList (NList _ l1)) dur2 (SummaryNList (NList _ l2)) =
    SummaryNList (NList undefined (zipWith a l1 l2))
    where
        a s1 s2 = appendSummaryData dur1 s1 dur2 s2

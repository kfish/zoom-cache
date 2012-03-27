{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
{- |
   Module      : Data.ZoomCache.Multichannel.NList
   Copyright   : Conrad Parker
   License     : BSD3-style (see LICENSE)

   Maintainer  : Conrad Parker <conrad@metadecks.org>
   Stability   : unstable
   Portability : unknown

Default codec implementation for multichannel values of type (NList n a).

This module implements the interfaces documented in "Data.ZoomCache.Codec".

Multichannel SummaryData is simply a concatenation of n blocks of SummaryData
for type a.

-}
----------------------------------------------------------------------

module Data.ZoomCache.Multichannel.NList (
      SummaryData(..)
    , SummaryWork(..)

    , summaryNListToList
    , summaryUTCNListToList
)where

import Blaze.ByteString.Builder
import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Iteratee (Iteratee)
import Data.List (intersperse)
import Data.Monoid (mconcat)
import Data.TypeLevel.Num hiding ((==))

import Data.ZoomCache.Codec
import Data.ZoomCache.Multichannel.Common
import Data.ZoomCache.NList
import Data.ZoomCache.Types

----------------------------------------------------------------------

summaryNListToList :: Summary (NList n a) -> [Summary a]
summaryNListToList s = map mkSummary (expandData (summaryData s))
    where
        mkSummary :: SummaryData a -> Summary a
        mkSummary sd = s { summaryData = sd }
        expandData :: SummaryData (NList n a) -> [SummaryData a]
        expandData (SummaryNList (NList _ l)) = l

summaryUTCNListToList :: SummaryUTC (NList n a) -> [SummaryUTC a]
summaryUTCNListToList s = map mkSummaryUTC (expandData (summaryUTCData s))
    where
        mkSummaryUTC :: SummaryData a -> SummaryUTC a
        mkSummaryUTC sd = s { summaryUTCData = sd }
        expandData :: SummaryData (NList n a) -> [SummaryData a]
        expandData (SummaryNList (NList _ l)) = l

----------------------------------------------------------------------
-- Read

instance (Nat n, ZoomReadable a) => ZoomReadable (NList n a) where
    data SummaryData (NList n a) = SummaryNList (NList n (SummaryData a))

    trackIdentifier = mkTrackTypeNList

    readRaw     = readNList
    readSummary = readSummaryNList

    prettyRaw         = prettyPacketNList
    prettySummaryData = prettySummaryNList

mkTrackTypeNList :: (Nat n, ZoomReadable a) => (NList n a) -> ByteString
mkTrackTypeNList (NList nv l) = mconcat
    [ trackTypeMultichannel
    , toByteString . fromIntegral32be . toInt $ nv
    , toByteString . fromIntegral32be . B.length $ subIdent
    , subIdent
    ]
    where
        subIdent = trackIdentifier (head l)

prettyPacketNList :: (Nat n, ZoomReadable a) => NList n a -> String
prettyPacketNList (NList _ l) = "[" ++ (concat $ intersperse "," (map prettyRaw l)) ++ "]"

readNList :: forall m n a. (Functor m, Monad m, Nat n, ZoomReadable a)
          => Iteratee ByteString m (NList n a)
readNList = NList unify <$> replicateM (toInt unify) readRaw
    where
        unify :: n
        unify = undefined

readSummaryNList :: forall m n a. (Functor m, Monad m, Nat n, ZoomReadable a)
                 => Iteratee ByteString m (SummaryData (NList n a))
readSummaryNList = SummaryNList .
    NList unify <$> replicateM (toInt unify) readSummary
    where
        unify :: n
        unify = undefined

prettySummaryNList :: (Nat n, ZoomReadable a) => SummaryData (NList n a) -> String
prettySummaryNList (SummaryNList (NList _ l)) =
    "[" ++ (concat $ intersperse "," (map prettySummaryData l)) ++ "]"

----------------------------------------------------------------------
-- Write

instance (Nat n, ZoomWrite a, ZoomWritable a) => ZoomWrite (NList n a) where
    write = writeData

instance (Nat n, ZoomWrite a, ZoomWritable a) => ZoomWrite (SampleOffset, (NList n a)) where
    write = writeDataVBR

instance (Nat n, ZoomWritable a) => ZoomWritable (NList n a) where
    data SummaryWork (NList n a) = SummaryWorkNList (NList n (SummaryWork a))

    fromRaw           = fromNList
    fromSummaryData   = fromSummaryNList

    initSummaryWork   = initSummaryNList
    toSummaryData     = mkSummaryNList
    updateSummaryData = updateSummaryNList
    appendSummaryData = appendSummaryNList

fromNList :: ZoomWritable a => (NList n a) -> Builder
fromNList (NList _ l) = mconcat $ map fromRaw l

initSummaryNList :: forall n a. (Nat n, ZoomWritable a)
                 => SampleOffset -> SummaryWork (NList n a)
initSummaryNList entry = SummaryWorkNList $
    NList unify (replicate (toInt unify) (initSummaryWork entry))
    where
        unify :: n
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

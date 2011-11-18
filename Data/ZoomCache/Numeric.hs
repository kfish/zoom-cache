{-# LANGUAGE CPP #-}
{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      : Data.ZoomCache.Types
-- Copyright   : Conrad Parker
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Conrad Parker <conrad@metadecks.org>
-- Stability   : unstable
-- Portability : unknown
--
-- ZoomCache numeric API
----------------------------------------------------------------------

module Data.ZoomCache.Numeric (
    ZoomNum
  , numEntry
  , numExit
  , numMin
  , numMax
  , numAvg
  , numRMS

  , toSummaryDouble

  , enumCacheFileSummaryDouble

  , module Data.ZoomCache
) where

import Control.Applicative ((<$>))
import Control.Monad.Trans (MonadIO)
import Data.ByteString (ByteString)
import Data.Int
import qualified Data.Iteratee as I
import Data.Maybe
import Data.Typeable
import Data.Word
import Data.ZoomCache
import Data.ZoomCache.Numeric.Types
import Data.ZoomCache.Types

----------------------------------------------------------------------

-- | Coercion of numeric summaries to type Summary Double.
toSummaryDouble :: Typeable a => Summary a -> Maybe (Summary Double)
toSummaryDouble s | typeOf s == typeOf (undefined :: Summary Double) =
                                id (cast s :: Maybe (Summary Double))
                  | typeOf s == typeOf (undefined :: Summary Float) =
                            sd <$> (cast s :: Maybe (Summary Float))
                  | typeOf s == typeOf (undefined :: Summary Int) =
                            sd <$> (cast s :: Maybe (Summary Int))
                  | typeOf s == typeOf (undefined :: Summary Int8) =
                            sd <$> (cast s :: Maybe (Summary Int8))
                  | typeOf s == typeOf (undefined :: Summary Int16) =
                            sd <$> (cast s :: Maybe (Summary Int16))
                  | typeOf s == typeOf (undefined :: Summary Int32) =
                            sd <$> (cast s :: Maybe (Summary Int32))
                  | typeOf s == typeOf (undefined :: Summary Int64) =
                            sd <$> (cast s :: Maybe (Summary Int64))
                  | typeOf s == typeOf (undefined :: Summary Integer) =
                            sd <$> (cast s :: Maybe (Summary Integer))
                  | typeOf s == typeOf (undefined :: Summary Word) =
                            sd <$> (cast s :: Maybe (Summary Word))
                  | typeOf s == typeOf (undefined :: Summary Word8) =
                            sd <$> (cast s :: Maybe (Summary Word8))
                  | typeOf s == typeOf (undefined :: Summary Word16) =
                            sd <$> (cast s :: Maybe (Summary Word16))
                  | typeOf s == typeOf (undefined :: Summary Word32) =
                            sd <$> (cast s :: Maybe (Summary Word32))
                  | typeOf s == typeOf (undefined :: Summary Word64) =
                            sd <$> (cast s :: Maybe (Summary Word64))
                  | otherwise = Nothing

    where
        sd :: ZoomNum a => Summary a -> Summary Double
        sd s' = s' { summaryData = toSummaryDataDouble (summaryData s') }
        
toSummaryDataDouble :: ZoomNum a => SummaryData a -> SummaryData Double
toSummaryDataDouble s = numMkSummary
    (realToFrac . numEntry $ s)
    (realToFrac . numExit $ s)
    (realToFrac . numMin $ s)
    (realToFrac . numMax $ s)
    (numAvg s)
    (numRMS s)

enumCacheFileSummaryDouble :: (Functor m, MonadIO m)
                           => [IdentifyCodec]
                           -> Int
                           -> I.Enumeratee ByteString [Summary Double] m a
enumCacheFileSummaryDouble mappings level = I.joinI .
    enumCacheFileSummaryLevel mappings level .
    I.mapChunks (catMaybes . map toSD)
    where
        toSD :: ZoomSummary -> Maybe (Summary Double)
        toSD (ZoomSummary s) = toSummaryDouble s

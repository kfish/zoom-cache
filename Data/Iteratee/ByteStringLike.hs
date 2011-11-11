{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      : Data.Iteratee.ByteStringLike
-- Copyright   : Conrad Parker
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Conrad Parker <conrad@metadecks.org>
-- Stability   : unstable
-- Portability : unknown
--
-- Conversion to and from ByteString-like iteratees.
----------------------------------------------------------------------

module Data.Iteratee.ByteStringLike (
    -- * ByteStringLike
      ByteStringLike(..)

    -- * Iteratee conversions
    , iterFromByteStringLike
    , iterToByteStringLike
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Iteratee (Iteratee)
import qualified Data.Iteratee as I
import qualified Data.ListLike as LL
import Data.Word

----------------------------------------------------------------------

class (I.Nullable s, LL.ListLike s Word8) => ByteStringLike s where
    likeFromByteString :: ByteString -> s
    likeToByteString   :: s -> ByteString

instance ByteStringLike [Word8] where
    likeFromByteString = B.unpack
    likeToByteString   = B.pack

instance ByteStringLike ByteString where
    likeFromByteString = id
    likeToByteString   = id

----------------------------------------------------------------------

-- | Convert an Iteratee on a ByteStringLike stream into
-- an Iteratee on a ByteString stream.
iterFromByteStringLike :: (I.Nullable s, LL.ListLike s Word8, ByteStringLike s, Monad m)
                       => Iteratee s m a -> Iteratee ByteString m a
iterFromByteStringLike = I.joinI . I.mapChunks likeFromByteString

-- | Convert an Iteratee on a ByteString stream into
-- an Iteratee on a ByteStringLike stream.
iterToByteStringLike :: (I.Nullable s, LL.ListLike s Word8, ByteStringLike s, Monad m)
                     => Iteratee ByteString m a -> Iteratee s m a
iterToByteStringLike = I.joinI . I.mapChunks likeToByteString

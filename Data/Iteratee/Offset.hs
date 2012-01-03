{-# OPTIONS -Wall #-}

module Data.Iteratee.Offset (
    -- * ListLike
      head
) where

import Prelude hiding (head)

import Data.ByteString (ByteString)
import Data.Iteratee (Iteratee)
import qualified Data.Iteratee as I
import qualified Data.ListLike as LL
import Data.Word

import Data.Offset

----------------------------------------------------------------------

head :: (Monad m)
     => Iteratee (Offset ByteString) m (Offset Word8)
head = I.liftI step
  where
  step (I.Chunk (Offset o vec))
    | LL.null vec = I.icont step Nothing
    | otherwise   = I.idone (Offset o (LL.head vec)) (I.Chunk $ Offset (o+1) (LL.tail vec))
  step stream     = I.icont step (Just (I.setEOF stream))


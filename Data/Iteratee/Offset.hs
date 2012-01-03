{-# OPTIONS -Wall #-}

module Data.Iteratee.Offset (
      tell

    -- * ListLike
    , head
    , peek
    , drop
) where

import Prelude hiding (drop, head)

import Data.ByteString (ByteString)
import Data.Iteratee (Iteratee)
import qualified Data.Iteratee as I
import qualified Data.ListLike as LL
import Data.Word
import System.Posix (FileOffset)

import Data.Offset

----------------------------------------------------------------------

tell :: (Monad m)
     => Iteratee (Offset ByteString) m FileOffset
tell = I.liftI step
  where
    step s@(I.Chunk (Offset o vec))
      | LL.null vec = I.liftI step
      | otherwise   = I.idone o s
    step stream     = I.icont step (Just (I.setEOF stream))

----------------------------------------------------------------------

head :: (Monad m)
     => Iteratee (Offset ByteString) m (Offset Word8)
head = I.liftI step
  where
  step (I.Chunk (Offset o vec))
    | LL.null vec = I.icont step Nothing
    | otherwise   = I.idone (Offset o (LL.head vec)) (I.Chunk $ Offset (o+1) (LL.tail vec))
  step stream     = I.icont step (Just (I.setEOF stream))

peek :: (Monad m)
     => Iteratee (Offset ByteString) m (Maybe (Offset Word8))
peek = I.liftI step
  where
    step s@(I.Chunk (Offset o vec))
      | LL.null vec = I.liftI step
      | otherwise   = I.idone (Just $ Offset o (LL.head vec)) s
    step stream     = I.idone Nothing stream

drop :: (Monad m)
     => Int -> Iteratee (Offset ByteString) m ()
drop 0  = return ()
drop n' = I.liftI (step n')
  where
    step n (I.Chunk (Offset o str))
      | LL.length str < n = I.liftI (step (n - LL.length str))
      | otherwise         = I.idone () (I.Chunk $ Offset (o + fromIntegral n) (LL.drop n str))
    step _ stream         = I.idone () stream

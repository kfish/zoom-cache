{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -Wall #-}

module Data.Iteratee.Offset (
      tell

    -- * Iteratee conversion
    , convOffset

    -- * ListLike
    , takeBS
    , head
    , offHead
    , peek
    , drop
) where

import Prelude hiding (drop, head)

import Control.Monad (liftM)
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Iteratee (Iteratee)
import qualified Data.Iteratee as I
import qualified Data.ListLike as LL
import Data.Monoid (mappend)
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

-- | Run a ByteString iteratee on an (Offset ByteString) input stream
convOffset :: (Monad m)
           => Iteratee ByteString m a
           -> Iteratee (Offset ByteString) m a
convOffset i = do
    n <- liftM fromIntegral tell
    go n (const i) (I.Chunk I.empty)
  where
    go :: (Monad m)
       => Integer
       -> (I.Stream ByteString -> Iteratee ByteString m a)
       -> I.Stream (Offset ByteString)
       -> Iteratee (Offset ByteString) m a
    go !n f str@(I.EOF _) = I.Iteratee rI
      where
        rI od oc = I.runIter (f (unwrapStream str)) onDone onCont
          where
            onDone a _ = od a str
            onCont f' mExc = oc (go n f') mExc
    go !n f str@(I.Chunk c) = I.Iteratee rI
      where
        newLen = n + fromIntegral (LL.length c)
        rI od oc = I.runIter (f (unwrapStream str)) onDone onCont
          where
            onDone a str'@(I.Chunk c') =
                let n' = newLen - fromIntegral (LL.length c') in
                od a (wrapStream n' str')
            onDone a str'@(I.EOF _) = od a (wrapStream n str')
            onCont f' mExc = oc (go newLen f') mExc
{-# INLINE convOffset #-}

unwrapStream :: I.Stream (Offset a) -> I.Stream a
unwrapStream (I.EOF e) = I.EOF e
unwrapStream (I.Chunk (Offset _ xs)) = I.Chunk xs

wrapStream :: Integral n => n -> I.Stream a -> I.Stream (Offset a)
wrapStream _ (I.EOF e) = I.EOF e
wrapStream n (I.Chunk xs) = I.Chunk (Offset (fromIntegral n) xs)

----------------------------------------------------------------------

takeBS :: (Monad m)
       => Int -> Iteratee (Offset ByteString) m ByteString
takeBS 0  = return B.empty
takeBS n' = I.liftI (step n' B.empty)
  where
    step n acc (I.Chunk (Offset o str))
      | LL.length str < n = I.liftI (step (n - LL.length str) (acc `mappend` str))
      | otherwise         = case LL.splitAt n str of
          (str', tail')
            | LL.null tail' -> I.icont (step (n - LL.length str') (acc `mappend` str')) Nothing
            | otherwise     -> I.idone (acc `mappend` str') (I.Chunk $ Offset (o + fromIntegral n) tail')
    step _ acc stream     = I.idone acc stream

----------------------------------------------------------------------

head :: (Monad m)
     => Iteratee (Offset ByteString) m Word8
head = liftM unwrapOffset offHead

offHead :: (Monad m)
        => Iteratee (Offset ByteString) m (Offset Word8)
offHead = I.liftI step
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

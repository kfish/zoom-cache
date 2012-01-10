{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall #-}

module Data.Offset (
    -- * Types
      Offset(..)

    -- * Functions
    , unwrapOffset
) where

import Data.Monoid
import Data.Nullable
import Data.NullPoint
import Data.Word
import qualified Data.ListLike.FoldableLL as LL
import qualified Data.ListLike as LL
import System.Posix.Types (FileOffset)

----------------------------------------------------------------------

data Offset a = Offset {-# UNPACK #-}!FileOffset !a

instance Nullable a => Nullable (Offset a) where
    nullC (Offset _ bs) = nullC bs

instance NullPoint a => NullPoint (Offset a) where
    empty = Offset 0 empty

instance Monoid a => Monoid (Offset a) where
    mempty = Offset 0 mempty
    mappend (Offset o1 s1) (Offset _ s2) = Offset o1 (s1 `mappend` s2)
    mconcat [] = mempty
    mconcat [x] = x
    mconcat ((Offset o x):xs) = Offset o (mconcat (x:(map unwrapOffset xs)))

instance LL.FoldableLL s el => LL.FoldableLL (Offset s) el where
    foldl = foldlO
    foldr = foldrO

foldlO :: LL.FoldableLL s el => (a -> el -> a) -> a -> Offset s -> a
foldlO f a (Offset _ xs) = LL.foldl f a xs

foldrO :: LL.FoldableLL s el => (el -> b -> b) -> b -> Offset s -> b
foldrO f b (Offset _ xs) = LL.foldr f b xs

instance LL.ListLike s Word8 => LL.ListLike (Offset s) Word8 where
    singleton = Offset 0 . LL.singleton
    head (Offset _ xs) = LL.head xs
    tail (Offset o xs) = Offset (o+1) (LL.tail xs)
    null (Offset _ xs) = LL.null xs
    length (Offset _ xs) = LL.length xs

----------------------------------------------------------------------

unwrapOffset :: Offset a -> a
unwrapOffset (Offset _ x) = x

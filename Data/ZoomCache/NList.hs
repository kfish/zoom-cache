{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
{- |
   Module      : Data.ZoomCache.NList
   Copyright   : Conrad Parker
   License     : BSD3-style (see LICENSE)

   Maintainer  : Conrad Parker <conrad@metadecks.org>
   Stability   : unstable
   Portability : unknown

Fixed-length lists.
-}
----------------------------------------------------------------------

module Data.ZoomCache.NList (
      NList(..)
    , nListToList
)where

import Control.Applicative ((<$>))
import Data.Typeable
import Data.TypeLevel.Num hiding ((==))
import Test.QuickCheck.Arbitrary

----------------------------------------------------------------------

data NList n a = NList n [a]
    deriving (Show)

instance Eq a => Eq (NList n a) where
    (NList _ a1) == (NList _ a2) = a1 == a2

instance Typeable a => Typeable (NList n a) where
#if __GLASGOW_HASKELL__ >= 702
    typeOf _ = mkTyConApp (mkTyCon3 "zoom-cache" "Data.ZoomCache.NList" "NList") [typeOf (undefined :: a)]
#else
    typeOf _ = mkTyConApp (mkTyCon "Data.ZoomCache.NList") [typeOf (undefined :: a)]
#endif

instance forall n a. (Nat n, Arbitrary a) => Arbitrary (NList n a) where
    arbitrary = NList unify <$> sequence [ arbitrary | _ <- [1..(toInt unify)] ]
        where
            unify :: n
            unify = undefined

nListToList :: NList n a -> [a]
nListToList (NList _ xs) = xs


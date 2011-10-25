{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Data.ZoomCache.Dynamic (
      PacketData(..)
    , SummaryData(..)
)where

import Data.Dynamic

import Data.ZoomCache.Types

----------------------------------------------------------------------
-- Read

instance ZoomRead Dynamic where
    data PacketData Dynamic = PDDynamic [Dynamic]

------------------------------------------------------------
-- Summary

instance ZoomSummary Dynamic where
    data SummaryData Dynamic = SummaryDynamic Dynamic
    prettySummaryData = prettySummaryDynamic
    -- typeOfSummaryData = const (typeOf (undefined :: Dynamic))

prettySummaryDynamic :: SummaryData Dynamic -> String
prettySummaryDynamic _ = "<<SummaryDynamic>>"


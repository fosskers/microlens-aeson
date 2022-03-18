{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module    :  Lens.Micro.Aeson.Internal
-- Copyright :  (c) Colin Woodbury 2015-2022, (c) Edward Kmett 2013-2014, (c) Paul Wilson 2012
-- License   :  BSD3
-- Maintainer:  Colin Woodbury <colingw@gmail.com>
--
-- These are stolen from `Lens.Micro.Platform` to avoid its dependencies.
-- They're altered to be specific to the Aeson context.
-- Creating instances for `microlens` typeclasses is generally warned
-- against, hence these instances are hidden here.

module Lens.Micro.Aeson.Internal where

import           Data.Aeson (Value(..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import           Lens.Micro.Internal

---

type instance Index Value = Key.Key

type instance IxValue Value = Value

-- | Can only index into the contents of an `Object`,
-- which is a `KM.KeyMap` `Value`.
instance Ixed Value where
  ix i f (Object o) = Object <$> ix i f o
  ix _ _ v          = pure v
  {-# INLINE ix #-}

-- Thu Oct 21 11:49:16 2021
-- Adapted from lens-aeson to account for the aeson-2 update.

type instance Index (KM.KeyMap v) = Key.Key

type instance IxValue (KM.KeyMap v) = v

instance Ixed (KM.KeyMap v) where
  ix = ixAt

instance At (KM.KeyMap v) where
  at k f = KM.alterF f k

instance Each (KM.KeyMap a) (KM.KeyMap b) a b where
  each = traversed
  {-# INLINE each #-}

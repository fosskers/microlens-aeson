{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module    :  Lens.Micro.Aeson.Internal
-- Copyright :  (c) Colin Woodbury 2015, (c) Edward Kmett 2013-2014, (c) Paul Wilson 2012
-- License   :  BSD3
-- Maintainer:  Colin Woodbury <colingw@gmail.com>
--
-- These are stolen from `Lens.Micro.Platform` to avoid its dependencies.
-- They're altered to be specific to the Aeson context.
-- Creating instances for `microlens` typeclasses is generally warned
-- against, hence these instances are hidden here.

module Lens.Micro.Aeson.Internal where

import Data.Aeson (Value(..))
import Data.HashMap.Lazy as HashMap
import Data.Text (Text)
import Data.Vector as V
import Lens.Micro.Internal

---

type instance Index Value = Text

type instance IxValue Value = Value

-- | Can only index into the contents of an `Object`,
-- which is a `HashMap`.
instance Ixed Value where
  ix i f (Object o) = Object <$> ix i f o
  ix _ _ v          = pure v
  {-# INLINE ix #-}

type instance Index   (HashMap Text Value) = Text

type instance IxValue (HashMap Text Value) = Value

-- | Straight-forward implementation.
instance Ixed (HashMap Text Value) where
  ix k f m = case HashMap.lookup k m of
    Just v  -> (\v' -> HashMap.insert k v' m) <$> f v
    Nothing -> pure m
  {-# INLINE ix #-}

type instance Index   (V.Vector a) = Int

type instance IxValue (V.Vector a) = a

-- | Also straight-forward. Only applicable for non-zero length `Vector`s.
instance Ixed (V.Vector a) where
  ix i f v
    | 0 <= i && i < V.length v = (\a -> v V.// [(i, a)]) <$> f (v V.! i)
    | otherwise = pure v
  {-# INLINE ix #-}

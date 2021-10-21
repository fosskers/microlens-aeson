{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module    :  Lens.Micro.Aeson.Internal
-- Copyright :  (c) Colin Woodbury 2015-2021, (c) Edward Kmett 2013-2014, (c) Paul Wilson 2012
-- License   :  BSD3
-- Maintainer:  Colin Woodbury <colingw@gmail.com>
--
-- These are stolen from `Lens.Micro.Platform` to avoid its dependencies.
-- They're altered to be specific to the Aeson context.
-- Creating instances for `microlens` typeclasses is generally warned
-- against, hence these instances are hidden here.

module Lens.Micro.Aeson.Internal where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif
import           Data.Aeson (Value(..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import           Data.HashMap.Lazy as HashMap
import           Data.Text (Text)
import           Data.Vector as V
import           Lens.Micro.Internal

---

type instance Index Value = Text

type instance IxValue Value = Value

-- | Can only index into the contents of an `Object`,
-- which is a `HashMap`.
instance Ixed Value where
  ix i f (Object o) = Object <$> ix (Key.fromText i) f o
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

-- Thu Oct 21 11:49:16 2021
-- Adapted from lens-aeson to account for the aeson-2 update.

type instance Index (KM.KeyMap v) = Key.Key

type instance IxValue (KM.KeyMap v) = v

instance Ixed (KM.KeyMap v) where
  ix i f m = case KM.lookup i m of
    Nothing -> pure m
    Just v  -> (\v' -> KM.insert i v' m) <$> f v

instance At (KM.KeyMap v) where
  at k f = KM.alterF f k

instance Each (KM.KeyMap a) (KM.KeyMap b) a b where
  each = traversed
  {-# INLINE each #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Aeson.Lens.Internal where

import Data.Aeson (Value(..))
import Data.HashMap.Lazy as HashMap
import Data.Text (Text)
import Data.Vector as V
import Lens.Micro.Internal

---

-- | Creating instances for `microlens` typeclasses is generally warned
-- against, hence these instances are hidden here.

type instance Index Value = Text

type instance IxValue Value = Value

instance Ixed Value where
  ix i f (Object o) = Object <$> ix i f o
  ix _ _ v          = pure v
  {-# INLINE ix #-}

-- These are stolen from `Lens.Micro.Platform` to avoid its dependencies.
-- They're altered to be specific to the Aeson context.
type instance Index   (HashMap Text Value) = Text

type instance IxValue (HashMap Text Value) = Value

instance Ixed (HashMap Text Value) where
  ix k f m = case HashMap.lookup k m of
    Just v  -> (\v' -> HashMap.insert k v' m) <$> f v
    Nothing -> pure m
  {-# INLINE ix #-}

type instance Index   (V.Vector a) = Int

type instance IxValue (V.Vector a) = a

instance Ixed (V.Vector a) where
  ix i f v
    | 0 <= i && i < V.length v = (\a -> v V.// [(i, a)]) <$> f (v V.! i)
    | otherwise = pure v
  {-# INLINE ix #-}

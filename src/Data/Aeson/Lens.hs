{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013-2014, (c) Paul Wilson 2012
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module also exports orphan @'Ixed' 'Value'@ and
-- @'Plated' 'Value'@ instances.
--------------------------------------------------------------------
module Data.Aeson.Lens
  (
  -- * Numbers
    AsNumber(..)
  , _Integral
  , nonNull
  -- * Primitive
  , Primitive(..)
  , AsPrimitive(..)
  -- * Objects and Arrays
  , AsValue(..)
  , key, members
  , nth, values
  -- * Decoding
  , AsJSON(..)
  ) where

import           Data.Aeson
import           Data.Aeson.Lens.Internal ()
import           Data.Aeson.Parser (value)
import           Data.Attoparsec.ByteString.Lazy (maybeResult, parse)
import qualified Data.ByteString as Strict
import           Data.ByteString.Lazy.Char8 as Lazy hiding (putStrLn)
import           Data.Data
import           Data.HashMap.Strict (HashMap)
import           Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific
import           Data.Text as Text
import qualified Data.Text.Encoding as StrictText
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import           Data.Vector (Vector)
import           Lens.Micro
import           Prelude hiding (null)

-- $setup
-- >>> import Data.ByteString.Char8 as Strict.Char8
-- >>> import qualified Data.Vector as Vector
-- >>> :set -XOverloadedStrings

------------------------------------------------------------------------------
-- Scientific Traversals
------------------------------------------------------------------------------

class AsNumber t where
  -- |
  -- >>> "[1, \"x\"]" ^? nth 0 . _Number
  -- Just 1.0
  --
  -- >>> "[1, \"x\"]" ^? nth 1 . _Number
  -- Nothing
  _Number :: Traversal' t Scientific
  default _Number :: AsPrimitive t => Traversal' t Scientific
  _Number = _Primitive . _Number
  {-# INLINE _Number #-}

  -- |
  -- Traversal into an 'Double' over a 'Value', 'Primitive' or 'Scientific'
  --
  -- >>> "[10.2]" ^? nth 0 . _Double
  -- Just 10.2
  _Double :: Traversal' t Double
  _Double = _Number . lens Scientific.toRealFloat (const realToFrac)
  {-# INLINE _Double #-}

  -- |
  -- Traversal into an 'Integer' over a 'Value', 'Primitive' or 'Scientific'
  --
  -- >>> "[10]" ^? nth 0 . _Integer
  -- Just 10
  --
  -- >>> "[10.5]" ^? nth 0 . _Integer
  -- Just 10
  --
  -- >>> "42" ^? _Integer
  -- Just 42
  _Integer :: Traversal' t Integer
  _Integer = _Number . lens floor (const fromIntegral)
  {-# INLINE _Integer #-}

instance AsNumber Value where
  _Number f (Number n) = Number <$> f n
  _Number _ v = pure v
  {-# INLINE _Number #-}

instance AsNumber Scientific where
  _Number = id
  {-# INLINE _Number #-}

instance AsNumber Strict.ByteString
instance AsNumber Lazy.ByteString
instance AsNumber Text
instance AsNumber LazyText.Text
instance AsNumber String

------------------------------------------------------------------------------
-- Conversion Traversals
------------------------------------------------------------------------------

-- | Access Integer 'Value's as Integrals.
--
-- >>> "[10]" ^? nth 0 . _Integral
-- Just 10
--
-- >>> "[10.5]" ^? nth 0 . _Integral
-- Just 10
_Integral :: (AsNumber t, Integral a) => Traversal' t a
_Integral = _Number . lens floor (const fromIntegral)
{-# INLINE _Integral #-}

------------------------------------------------------------------------------
-- Null values and primitives
------------------------------------------------------------------------------

-- | Primitives of 'Value'
data Primitive
  = StringPrim !Text
  | NumberPrim !Scientific
  | BoolPrim !Bool
  | NullPrim
  deriving (Eq,Ord,Show,Data,Typeable)

instance AsNumber Primitive where
  _Number f (NumberPrim n) = NumberPrim <$> f n
  _Number _ p = pure p
  {-# INLINE _Number #-}

class AsNumber t => AsPrimitive t where
  -- |
  -- >>> "[1, \"x\", null, true, false]" ^? nth 0 . _Primitive
  -- Just (NumberPrim 1.0)
  --
  -- >>> "[1, \"x\", null, true, false]" ^? nth 1 . _Primitive
  -- Just (StringPrim "x")
  --
  -- >>> "[1, \"x\", null, true, false]" ^? nth 2 . _Primitive
  -- Just NullPrim
  --
  -- >>> "[1, \"x\", null, true, false]" ^? nth 3 . _Primitive
  -- Just (BoolPrim True)
  --
  -- >>> "[1, \"x\", null, true, false]" ^? nth 4 . _Primitive
  -- Just (BoolPrim False)
  _Primitive :: Traversal' t Primitive
  default _Primitive :: AsValue t => Traversal' t Primitive
  _Primitive = _Value . _Primitive
  {-# INLINE _Primitive #-}

  -- |
  -- >>> "{\"a\": \"xyz\", \"b\": true}" ^? key "a" . _String
  -- Just "xyz"
  --
  -- >>> "{\"a\": \"xyz\", \"b\": true}" ^? key "b" . _String
  -- Nothing
  _String :: Traversal' t Text
  _String = _Primitive . trav
    where trav f (StringPrim s) = StringPrim <$> f s
          trav _ x = pure x
  {-# INLINE _String #-}

  -- |
  -- >>> "{\"a\": \"xyz\", \"b\": true}" ^? key "b" . _Bool
  -- Just True
  --
  -- >>> "{\"a\": \"xyz\", \"b\": true}" ^? key "a" . _Bool
  -- Nothing
  _Bool :: Traversal' t Bool
  _Bool = _Primitive . trav
    where trav f (BoolPrim b) = BoolPrim <$> f b
          trav _ x = pure x
  {-# INLINE _Bool #-}

  -- |
  -- >>> "{\"a\": \"xyz\", \"b\": null}" ^? key "b" . _Null
  -- Just ()
  --
  -- >>> "{\"a\": \"xyz\", \"b\": null}" ^? key "a" . _Null
  -- Nothing
  _Null :: Traversal' t ()
  _Null = _Primitive . trav
    where trav f NullPrim = const NullPrim <$> f ()
          trav _ x = pure x
  {-# INLINE _Null #-}

-- Helper for the function below.
fromPrim :: Primitive -> Value
fromPrim (StringPrim s) = String s
fromPrim (NumberPrim n) = Number n
fromPrim (BoolPrim b)   = Bool b
fromPrim NullPrim       = Null
{-# INLINE fromPrim #-}

instance AsPrimitive Value where
  _Primitive f (String s) = fromPrim <$> f (StringPrim s)
  _Primitive f (Number n) = fromPrim <$> f (NumberPrim n)
  _Primitive f (Bool b)   = fromPrim <$> f (BoolPrim b)
  _Primitive f Null       = fromPrim <$> f NullPrim
  _Primitive _ v          = pure v
  {-# INLINE _Primitive #-}

  _String f (String s) = String <$> f s
  _String _ v = pure v
  {-# INLINE _String #-}

  _Bool f (Bool b) = Bool <$> f b
  _Bool _ v = pure v
  {-# INLINE _Bool #-}

  _Null f Null = const Null <$> f ()
  _Null _ v = pure v
  {-# INLINE _Null #-}

instance AsPrimitive Strict.ByteString
instance AsPrimitive Lazy.ByteString
instance AsPrimitive Text.Text
instance AsPrimitive LazyText.Text
instance AsPrimitive String

instance AsPrimitive Primitive where
  _Primitive = id
  {-# INLINE _Primitive #-}

-- | Traversal into non-'Null' values
--
-- >>> "{\"a\": \"xyz\", \"b\": null}" ^? key "a" . nonNull
-- Just (String "xyz")
--
-- >>> "{\"a\": {}, \"b\": null}" ^? key "a" . nonNull
-- Just (Object (fromList []))
--
-- >>> "{\"a\": \"xyz\", \"b\": null}" ^? key "b" . nonNull
-- Nothing
nonNull :: Traversal' Value Value
nonNull _ Null = pure Null
nonNull f v = _Value f v
{-# INLINE nonNull #-}

------------------------------------------------------------------------------
-- Non-primitive traversals
------------------------------------------------------------------------------

class AsPrimitive t => AsValue t where
  -- | Traverse into data that encodes a `Value`
  _Value :: Traversal' t Value

  -- |
  -- >>> "{\"a\": {}, \"b\": null}" ^? key "a" . _Object
  -- Just (fromList [])
  --
  -- >>> "{\"a\": {}, \"b\": null}" ^? key "b" . _Object
  -- Nothing
  _Object :: Traversal' t (HashMap Text Value)
  _Object = _Value . trav
    where trav f (Object o) = Object <$> f o
          trav _ v = pure v
  {-# INLINE _Object #-}

  _Array :: Traversal' t (Vector Value)
  _Array = _Value . trav
    where trav f (Array a) = Array <$> f a
          trav _ v = pure v
  {-# INLINE _Array #-}

instance AsValue Value where
  _Value = id
  {-# INLINE _Value #-}

instance AsValue Strict.ByteString where
  _Value = _JSON
  {-# INLINE _Value #-}

instance AsValue Lazy.ByteString where
  _Value = _JSON
  {-# INLINE _Value #-}

instance AsValue String where
  _Value = strictUtf8 . _JSON
  {-# INLINE _Value #-}

instance AsValue Text where
  _Value = strictTextUtf8 . _JSON
  {-# INLINE _Value #-}

instance AsValue LazyText.Text where
  _Value = lazyTextUtf8 . _JSON
  {-# INLINE _Value #-}

-- |
-- Like 'ix', but for 'Object' with Text indices. This often has better
-- inference than 'ix' when used with OverloadedStrings.
--
-- >>> "{\"a\": 100, \"b\": 200}" ^? key "a"
-- Just (Number 100.0)
--
-- >>> "[1,2,3]" ^? key "a"
-- Nothing
key :: AsValue t => Text -> Traversal' t Value
key i = _Object . ix i
{-# INLINE key #-}

-- | A Traversal into Object properties
--
-- >>> "{\"a\": 4, \"b\": 7}" ^.. members
-- [Number 4.0,Number 7.0]
--
-- >>> "{\"a\": 4, \"b\": 7}" & members . _Number *~ 10
-- "{\"a\":40,\"b\":70}"
members :: AsValue t => Traversal' t Value
members = _Object . traverse
{-# INLINE members #-}

-- | Like 'ix', but for Arrays with Int indexes
--
-- >>> "[1,2,3]" ^? nth 1
-- Just (Number 2.0)
--
-- >>> "\"a\": 100, \"b\": 200}" ^? nth 1
-- Nothing
--
-- >>> "[1,2,3]" & nth 1 .~ Number 20
-- "[1,20,3]"
nth :: AsValue t => Int -> Traversal' t Value
nth i = _Array . ix i
{-# INLINE nth #-}

-- | A Traversal into Array elements
--
-- >>> "[1,2,3]" ^.. values
-- [Number 1.0,Number 2.0,Number 3.0]
--
-- >>> "[1,2,3]" & values . _Number *~ 10
-- "[10,20,30]"
values :: AsValue t => Traversal' t Value
values = _Array . traverse
{-# INLINE values #-}

strictUtf8 :: Lens' String Strict.ByteString
strictUtf8 = lens Text.pack (const Text.unpack) . strictTextUtf8

lazyUtf8 :: Lens' Strict.ByteString Lazy.ByteString
lazyUtf8 = lens Lazy.fromStrict (const Lazy.toStrict)

strictTextUtf8 :: Lens' Text.Text Strict.ByteString
strictTextUtf8 = lens StrictText.encodeUtf8 (const StrictText.decodeUtf8)

lazyTextUtf8 :: Lens' LazyText.Text Lazy.ByteString
lazyTextUtf8 = lens LazyText.encodeUtf8 (const LazyText.decodeUtf8)

class AsJSON t where
  -- | '_JSON' is a 'Traversal' from something containing JSON
  -- to something encoded in that structure.
  _JSON :: Traversal' t Value

instance AsJSON Strict.ByteString where
  _JSON = lazyUtf8 . _JSON
  {-# INLINE _JSON #-}

instance AsJSON Lazy.ByteString where
  _JSON f b = case maybeResult (parse value b) of
    Just v -> encode <$> f v
    _      -> pure b
  {-# INLINE _JSON #-}

instance AsJSON String where
  _JSON = strictUtf8 . _JSON
  {-# INLINE _JSON #-}

instance AsJSON Text where
  _JSON = strictTextUtf8 . _JSON
  {-# INLINE _JSON #-}

instance AsJSON LazyText.Text where
  _JSON = lazyTextUtf8 . _JSON
  {-# INLINE _JSON #-}

instance AsJSON Value where
  _JSON = id
  {-# INLINE _JSON #-}

------------------------------------------------------------------------------
-- Some additional tests for prismhood; see https://github.com/ekmett/lens/issues/439.
------------------------------------------------------------------------------

-- $LazyByteStringTests
-- >>> "42" ^? (_JSON :: Prism' Lazy.ByteString Value)
-- Just (Number 42.0)
--
-- >>> preview (_Integer :: Prism' Lazy.ByteString Integer) "42"
-- Just 42
--
-- >>> Lazy.unpack (review (_Integer :: Prism' Lazy.ByteString Integer) 42)
-- "42"

-- $StrictByteStringTests
-- >>> "42" ^? (_JSON :: Prism' Strict.ByteString Value)
-- Just (Number 42.0)
--
-- >>> preview (_Integer :: Prism' Strict.ByteString Integer) "42"
-- Just 42
--
-- >>> Strict.Char8.unpack (review (_Integer :: Prism' Strict.ByteString Integer) 42)
-- "42"

-- $StringTests
-- >>> "42" ^? (_JSON :: Prism' String Value)
-- Just (Number 42.0)
--
-- >>> preview (_Integer :: Prism' String Integer) "42"
-- Just 42
--
-- >>> review (_Integer :: Prism' String Integer) 42
-- "42"


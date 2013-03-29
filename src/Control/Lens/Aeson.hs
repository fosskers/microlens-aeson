{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013, (c) Paul Wilson 2012
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Control.Lens.Aeson
  ( _Object
  , _Array
  , AsNumber(..)
  , integralValue
  , nonNull
  , AsPrimitive(..)
  , key
  , nth
  , decoded
  ) where

import Control.Lens
import Data.Aeson
import Data.Attoparsec.Number
import Data.ByteString.Lazy (ByteString)
import Data.Data
import Data.HashMap.Strict (HashMap)
import Data.Text
import Numeric.Lens
import Prelude hiding(null)
import Data.Vector (Vector)

------------------------------------------------------------------------------
-- Value prisms
------------------------------------------------------------------------------

_Object :: Prism' Value (HashMap Text Value)
_Object = prism Object $ \v -> case v of Object o -> Right o; _ -> Left v

_Array :: Prism' Value (Vector Value)
_Array = prism Array $ \v -> case v of Array a -> Right a; _ -> Left v

------------------------------------------------------------------------------
-- Number prisms
------------------------------------------------------------------------------

class AsNumber t where
  _Number :: Prism' t Number

  -- | Prism into an 'Double' over a 'Value', 'Primitive' or 'Number'
  _Double :: Prism' t Double
  _Double = _Number.prism D (\v -> case v of D d -> Right d; _ -> Left v)

  -- | Prism into an 'Integer' over a 'Value', 'Primitive' or 'Number'
  _Integer :: Prism' t Integer
  _Integer = _Number.prism I (\v -> case v of I i -> Right i; _ -> Left v)

instance AsNumber Value where
  _Number = prism Number $ \v -> case v of Number n -> Right n; _ -> Left v

instance AsNumber Number where
  _Number = id

------------------------------------------------------------------------------
-- Conversion Prisms
------------------------------------------------------------------------------

-- | Access Integer 'Value's as Integrals.
--
-- defined as `integer . 'Numeric.Lens.integral'`
integralValue :: (AsNumber t, Integral a) => Prism' t a
integralValue = _Integer . integral

------------------------------------------------------------------------------
-- Null values and primitives
------------------------------------------------------------------------------

-- | Primitives of 'Value'
data Primitive
  = StringPrim !Text
  | NumberPrim !Number
  | BoolPrim !Bool
  | NullPrim
  deriving (Eq,Ord,Show,Data,Typeable)

instance AsNumber Primitive where
  _Number = prism NumberPrim $ \v -> case v of NumberPrim s -> Right s; _ -> Left v

class AsNumber t => AsPrimitive t where
  _Primitive :: Prism' t Primitive

  _String :: Prism' t Text
  _String = _Primitive.prism StringPrim (\v -> case v of StringPrim s -> Right s; _ -> Left v)

  _Bool :: Prism' t Bool
  _Bool = _Primitive.prism BoolPrim (\v -> case v of BoolPrim b -> Right b; _ -> Left v)

  _Null :: Prism' t ()
  _Null = _Primitive.prism (const NullPrim) (\v -> case v of NullPrim -> Right (); _ -> Left v)

instance AsPrimitive Value where
  _Primitive = prism fromPrim toPrim
    where
      toPrim (String s) = Right $ StringPrim s
      toPrim (Number n) = Right $ NumberPrim n
      toPrim (Bool b)   = Right $ BoolPrim b
      toPrim Null       = Right $ NullPrim
      toPrim v          = Left v
      fromPrim (StringPrim s) = String s
      fromPrim (NumberPrim n) = Number n
      fromPrim (BoolPrim b)   = Bool b
      fromPrim NullPrim       = Null

  _String = prism String $ \v -> case v of String s -> Right s; _ -> Left v
  _Bool = prism Bool (\v -> case v of Bool b -> Right b; _ -> Left v)
  _Null = prism (const Null) (\v -> case v of Null -> Right (); _ -> Left v)

instance AsPrimitive Primitive where
  _Primitive = id

-- | Prism into non-'Null' values
nonNull :: Prism' Value Value
nonNull = prism id (\v -> if isn't _Null v then Right v else Left v)

------------------------------------------------------------------------------
-- Non-primitive traversals
------------------------------------------------------------------------------

-- | Traversal over a specific field of an object.
key :: Text -> IndexedTraversal' Text Value Value
key k = _Object . ix k

-- | Like 'key', but for Arrays with Int indexes
nth :: Int -> IndexedTraversal' Int Value Value
nth i = _Array . ix i

-- | A Prism into 'Value' on lazy 'ByteString's.
-- To illustrate (assuming the OverloadedStrings extension):
--
-- @
-- > over (decoded . key "a" . integer) (*100) $ "{\"a\": 1, \"b\": 3}"
-- "{\"b\":3,\"a\":100}"
-- @
decoded :: (FromJSON a, ToJSON a) => Prism' ByteString a
decoded = prism encode (\t -> maybe (Left t) Right $ decode t)

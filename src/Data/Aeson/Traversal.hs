{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Aeson.Traversal
  ( module Data.Aeson.Traversal.Value
  , module Data.Aeson.Traversal.Primitive
  , integer
  , double
  , integralv
  , nonNull
  , primitive
  , primitive'
  , key
  , nth
  , decoded
  ) where

import Prelude hiding(null)

-- Utils & lens
import Control.Applicative (Applicative)
import Control.Lens
import Control.Lens.Traversal
import Control.Lens.TH
import Numeric.Lens

-- Types used by aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text(..))
import Data.Vector (Vector(..))
import qualified Data.Vector as V (fromList)
import Data.HashMap.Strict (HashMap(..), fromList)
import Data.Attoparsec.Number

-- Aeson main library
import Data.Aeson
import Data.Aeson.TH

import Data.Aeson.Traversal.Value
import Data.Aeson.Traversal.Primitive

{-- Conversion Prisms --}

-- | Prism into an 'Integer' over a 'Value'
--
-- defined as `_Number . _I`.
integer :: Prism' Value Integer
integer = _Number . _I

-- | Prism into Double over Value
--
-- defined as `_Number . _D`.
double :: Prism' Value Double
double = _Number . _D

-- | Access Integer 'Value's as Integrals.
--
-- defined as `integer . 'Numeric.Lens.integral'`
integralv :: Integral a => Prism' Value a
integralv = integer . integral

{-- Null values & primitives --}

-- | Prism into non-'Null' values
nonNull :: Prism' Value Value
nonNull = prism id (\v -> if isn't _Null v then Right v else Left v)

-- | 'Primitive' 'Value's- i.e. things that aren't
-- 'Object' or 'Array'.
primitive :: Prism' Value Primitive
primitive = prism fromPrim toPrim

-- | Like 'primitive', but into 'Value' instead of 'Primitive'
primitive' :: Prism' Value Value
primitive' = prism id f
  where f v@(Object _) = Left v
        f v@(Array  _) = Left v
        f v            = Right v

{-- non-primitive traversals --}

-- | Traversal over a specific field of an object.
key :: Text -> Traversal' Value Value
key k = _Object . at k . _Just

-- | Like 'key', but for Arrays with Int indexes
nth :: Int -> Traversal' Value Value
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

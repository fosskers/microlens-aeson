{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RankNTypes        #-}

module Data.Aeson.Prism where

import Prelude hiding (null)

-- Utils & lens
import Control.Applicative (Applicative)
import Control.Lens (Prism, Prism', isn't)
import Control.Lens.Prism (prism)
import Numeric.Lens (integral)

-- Types used by aeson
import Data.Text (Text(..))
import Data.Vector (Vector(..))
import Data.HashMap.Strict (HashMap(..))
import Data.Attoparsec.Number

-- Aeson main library
import Data.Aeson

-- Derived prisms
import Data.Aeson.Traversal.Derived
import Data.Aeson.Traversal.Types

{-- Basic prisms for Value --}

_object :: Prism' Value (HashMap Text Value)
_object = _Object

_array :: Prism' Value (Vector Value)
_array = _Array

_string :: Prism' Value Text
_string = _String

_number :: Prism' Value Number
_number = _Number

_bool :: Prism' Value Bool
_bool = _Bool

-- | null value
_null :: Prism' Value ()
_null = _Null

{-- Prisms for attoparsec's Number type --}

-- | Attoparsec Number  prisms
integer :: Prism' Value Integer
integer = _number . _I

-- | Attoparsec Number Double prism
double :: Prism' Value Double
double = _number . _D

{-- Conversions --}

-- | Access Integer 'Value's as Integrals
integralv :: Integral a => Prism' Value a
integralv = integer . integral

{-- More general kinds of Value --}

-- | Non-'Null' values
nonNull :: Prism' Value Value
nonNull = prism id (\v -> if isn't _null v then Right v else Left v)

-- | Primitive 'Value's- i.e. things that aren't an 'Object' 
-- or 'Array'.
primitive :: Prism' Value Primitive
primitive = prism fromPrim toPrim

-- | Like 'primitive', but with Values instead of Primitives.
primitive' :: Prism' Value Value
primitive' = prism id f
  where f v@(Object _) = Left v
        f v@(Array  _) = Left v
        f v            = Right v

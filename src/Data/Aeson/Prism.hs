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
import Data.Aeson.Traversal.Value
import Data.Aeson.Traversal.Primitive

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

-- | Like 'primitive', but returning Values instead of Primitives.
primitive' :: Prism' Value Value
primitive' = prism id f
  where f v@(Object _) = Left v
        f v@(Array  _) = Left v
        f v            = Right v

{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RankNTypes        #-}

-- | You probably shouldn't be using this module.
-- The prisms it exports are renamed in Data.Aeson.Prism
module Data.Aeson.Traversal.Value
  ( _Object
  , _Array
  , _String
  , _Number
  , _Bool
  , _Null
  , _I
  , _D
  ) where

import Control.Lens (Prism, Prism')
import Control.Lens.Prism (prism)
import Control.Lens.TH
import Data.Aeson (Value(..))

import Data.Text (Text(..))
import Data.Vector (Vector(..))
import Data.HashMap.Strict (HashMap(..))
import Data.Attoparsec.Number (Number(..))

{-- Value prisms --}

_Object :: Prism' Value (HashMap Text Value)
_Object = prism Object $ \v -> case v of Object o -> Right o; _ -> Left v

_Array :: Prism' Value (Vector Value)
_Array = prism Array $ \v -> case v of Array a -> Right a; _ -> Left v

_String :: Prism' Value Text
_String = prism String $ \v -> case v of String s -> Right s; _ -> Left v

_Number :: Prism' Value Number
_Number = prism Number $ \v -> case v of Number n -> Right n; _ -> Left v

_Bool :: Prism' Value Bool
_Bool = prism Bool $ \v -> case v of Bool b -> Right b; _ -> Left v

_Null :: Prism' Value ()
_Null = prism (const Null) $ \v -> case v of Null -> Right (); _ -> Left v

{-- Number prisms --}

_D :: Prism' Number Double
_D = prism D $ \v -> case v of D d -> Right d; _ -> Left v

_I :: Prism' Number Integer
_I = prism I $ \v -> case v of I i -> Right i; _ -> Left v

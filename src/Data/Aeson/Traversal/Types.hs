module Data.Aeson.Traversal.Types where

import Data.Text(Text)
import Data.Attoparsec.Number(Number)

-- Aeson main library
import Data.Aeson

-- | Primitives from 'Value'
data Primitive
  = StringP !Text
  | NumberP !Number
  | BoolP   !Bool
  | NullP

toPrim :: Value -> Either Value Primitive
toPrim (String s) = Right $ StringP s
toPrim (Number n) = Right $ NumberP n
toPrim (Bool b)   = Right $ BoolP b
toPrim Null       = Right $ NullP
toPrim v          = Left v

fromPrim :: Primitive -> Value
fromPrim (StringP s) = String s
fromPrim (NumberP n) = Number n
fromPrim (BoolP b)   = Bool b
fromPrim NullP       = Null

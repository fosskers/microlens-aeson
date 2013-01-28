module Data.Aeson.Traversal.Primitive where

import Data.Text(Text)
import Data.Attoparsec.Number(Number)

-- Aeson main library
import Data.Aeson

-- | Primitives of 'Value'
data Primitive
  = StringP !Text
  | NumberP !Number
  | BoolP   !Bool
  | NullP

-- | Try to convert a Value to a Primitive
toPrim :: Value -> Either Value Primitive
toPrim (String s) = Right $ StringP s
toPrim (Number n) = Right $ NumberP n
toPrim (Bool b)   = Right $ BoolP b
toPrim Null       = Right $ NullP
toPrim v          = Left v

-- | Safely convert a Primitive back into a Value
fromPrim :: Primitive -> Value
fromPrim (StringP s) = String s
fromPrim (NumberP n) = Number n
fromPrim (BoolP b)   = Bool b
fromPrim NullP       = Null

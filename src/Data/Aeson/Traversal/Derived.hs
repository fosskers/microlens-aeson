{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RankNTypes        #-}

-- | You probably shouldn't be using this module.
-- The prisms it exports are renamed in Data.Aeson.Prism
module Data.Aeson.Traversal.Derived
  ( _Object
  , _Array
  , _String
  , _Number
  , _Bool
  , _Null
  , _I
  , _D
  ) where

import Control.Lens.TH
import Data.Aeson (Value)
import Data.Attoparsec.Number (Number)

-- Use TH to make simple prisms for value.
makePrisms ''Value

-- Similarly for Number
makePrisms ''Number

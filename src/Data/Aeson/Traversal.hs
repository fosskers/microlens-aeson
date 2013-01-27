{-# LANGUAGE TemplateHaskell   #-}
module Data.Aeson.Traversal where

import Prelude hiding(null)

-- Utils & lens
import Control.Applicative (Applicative)
import Control.Lens
import Control.Lens.Traversal
import Control.Lens.TH

-- Types used by aeson
import Data.Text (Text(..))
import Data.Vector (Vector(..))
import Data.HashMap.Strict (HashMap(..))
import Data.Attoparsec.Number

-- Aeson main library
import Data.Aeson
import Data.Aeson.TH

import Data.Aeson.Prism
import Data.Aeson.Traversal.Types

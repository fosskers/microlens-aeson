{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Lens as L
import           Criterion.Main
import qualified Data.Aeson.Lens as AL
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Lens.Micro
import           Lens.Micro.Aeson

---

main :: IO ()
main = defaultMain
  [ bgroup "First elem in Array"
    [ bench "microlens-aeson (String)"  $ nf (^? nth 0 . _Number) ("[1, \"x\"]" :: String)
    , bench "microlens-aeson (T.Text)"  $ nf (^? nth 0 . _Number) ("[1, \"x\"]" :: T.Text)
    , bench "microlens-aeson (TL.Text)" $ nf (^? nth 0 . _Number) ("[1, \"x\"]" :: TL.Text)
    , bench "microlens-aeson (BS)"      $ nf (^? nth 0 . _Number) ("[1, \"x\"]" :: BS.ByteString)
    , bench "microlens-aeson (BL)"      $ nf (^? nth 0 . _Number) ("[1, \"x\"]" :: BL.ByteString)
    , bench "lens-aeson (String)"  $ nf (L.^? AL.nth 0 . AL._Number) ("[1, \"x\"]" :: String)
    , bench "lens-aeson (T.Text)"  $ nf (L.^? AL.nth 0 . AL._Number) ("[1, \"x\"]" :: T.Text)
    , bench "lens-aeson (TL.Text)" $ nf (L.^? AL.nth 0 . AL._Number) ("[1, \"x\"]" :: TL.Text)
    , bench "lens-aeson (BS)"      $ nf (L.^? AL.nth 0 . AL._Number) ("[1, \"x\"]" :: BS.ByteString)
    , bench "lens-aeson (BL)"      $ nf (L.^? AL.nth 0 . AL._Number) ("[1, \"x\"]" :: BL.ByteString) ]
  , bgroup "Getting via `key`"
    [ bench "microlens-aeson (Text)" $ nf (^? key "a")      ("{\"a\": 100, \"b\": 200}" :: T.Text)
    , bench "lens-aeson (Text)"      $ nf (L.^? AL.key "a") ("{\"a\": 100, \"b\": 200}" :: T.Text) ]
  , bgroup "Setting via `key`"
    [ bench "microlens-aeson (Text)" $ nf (& key "a" . _Number %~ (* 10)) ("{\"a\": 100, \"b\": 200}" :: T.Text)
    , bench "lens-aeson (Text)" $ nf (& AL.key "a" . AL._Number L.%~ (* 10)) ("{\"a\": 100, \"b\": 200}" :: T.Text) ]
  ]

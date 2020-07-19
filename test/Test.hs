module Main where

import           Data.Aeson
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import           Lens.Micro
import           Lens.Micro.Aeson
import           Test.Tasty
import           Test.Tasty.HUnit
import           Data.List

---

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "Unit Tests"
  [ testGroup "Scientific Traversals"
    [ testCase "" $ ("[1, \"x\"]" ^? nth 0 . _Number) @?= Just 1.0
    , testCase "" $ ("[1, \"x\"]" ^? nth 1 . _Number) @?= Nothing
    , testCase "" $ ("[10.2]" ^? nth 0 . _Double) @?= Just 10.2
    , testCase "" $ ("[10]" ^? nth 0 . _Integer) @?= Just 10
    , testCase "" $ ("[10.5]" ^? nth 0 . _Integer) @?= Just 10
    , testCase "" $ ("42" ^? _Integer) @?= Just 42
    ]
  , testGroup "Conversion Traversals"
    [ testCase "" $ ("[10]" ^? nth 0 . _Integral) @?= Just (10 :: Int)
    , testCase "" $ ("[10.5]" ^? nth 0 . _Integral) @?= Just (10 :: Int)
    ]
  , testGroup "Nulls and Primitives"
    [ testCase "" $ ("[1, \"x\", null, true, false]" ^? nth 0 . _Primitive) @?= Just (NumberPrim 1.0)
    , testCase "" $ ("[1, \"x\", null, true, false]" ^? nth 1 . _Primitive) @?= Just (StringPrim $ T.pack "x")
    , testCase "" $ ("[1, \"x\", null, true, false]" ^? nth 2 . _Primitive) @?= Just NullPrim
    , testCase "" $ ("[1, \"x\", null, true, false]" ^? nth 3 . _Primitive) @?= Just (BoolPrim True)
    , testCase "" $ ("[1, \"x\", null, true, false]" ^? nth 4 . _Primitive) @?= Just (BoolPrim False)
    , testCase "" $ ("{\"a\": \"xyz\", \"b\": true}" ^? key (T.pack "a") . _String) @?= Just (T.pack "xyz")
    , testCase "" $ ("{\"a\": \"xyz\", \"b\": true}" ^? key (T.pack "b") . _String) @?= Nothing
    , testCase "" $ ("{\"a\": \"xyz\", \"b\": true}" ^? key (T.pack "b") . _Bool) @?= Just True
    , testCase "" $ ("{\"a\": \"xyz\", \"b\": true}" ^? key (T.pack "a") . _Bool) @?= Nothing
    , testCase "" $ ("{\"a\": \"xyz\", \"b\": null}" ^? key (T.pack "b") . _Null) @?= Just ()
    , testCase "" $ ("{\"a\": \"xyz\", \"b\": null}" ^? key (T.pack "a") . _Null) @?= Nothing
    , testCase "" $ ("{\"a\": \"xyz\", \"b\": null}" ^? key (T.pack "a") . nonNull) @?= Just (String $ T.pack "xyz")
    , testCase "" $ ("{\"a\": {}, \"b\": null}" ^? key (T.pack "a") . nonNull) @?= Just (Object (HMS.fromList []))
    , testCase "" $ ("{\"a\": \"xyz\", \"b\": null}" ^? key (T.pack "b") . nonNull) @?= Nothing
    ]
  , testGroup "Non-primitive Traversals"
    [ testCase "" $ ("{\"a\": {}, \"b\": null}" ^? key (T.pack "a") . _Object) @?= Just (HMS.fromList [])
    , testCase "" $ ("{\"a\": {}, \"b\": null}" ^? key (T.pack "b") . _Object) @?= Nothing
    , testCase "" $ ("{\"a\": 100, \"b\": 200}" ^? key (T.pack "a")) @?= Just (Number 100.0)
    , testCase "" $ ("[1,2,3]" ^? key (T.pack "a")) @?= Nothing
    , testCase "" $ (sort ("{\"a\": 4, \"b\": 7}" ^.. members . _Number)) @?= [4.0, 7.0]
    , testCase "" $ ("{\"a\": 4}" & members . _Number %~ (* 10)) @?= "{\"a\":40}"
    , testCase "" $ ("[1,2,3]" ^? nth 1) @?= Just (Number 2.0)
    , testCase "" $ ("{\"a\": 100, \"b\": 200}" ^? nth 1) @?= Nothing
    , testCase "" $ ("[1,2,3]" & nth 1 .~ Number 20) @?= "[1,20,3]"
    , testCase "" $ ("[1,2,3]" ^.. values) @?= [Number 1.0,Number 2.0,Number 3.0]
    , testCase "" $ ("[1,2,3]" & values . _Number %~ (* 10)) @?= "[10,20,30]"
    ]
  ]

# microlens-aeson

## 2.3.1 (2020-07-19)

#### Fixed

- Test suite fixed to succeed deterministically on 32-bit machines.

## 2.3.0.3
- Readd 7.10 support. Forgive the spam.

## 2.3.0.2
- Bumping bounds, cleaning code.

## 2.3.0.1
- Enable compilation with GHC `7.8` (`base-4.7`). Only when this version of
  `base` is used does our type `Primitive` lose its `Hashable` instance, due to
  a lack of `DeriveAnyClass`. Otherwise, the API is unchanged from
  `microlens-aeson-2.3.0`.

## 2.2.0
- Various fixes to dependency version bounds

## 2.1.0
* Restored original `AsJSON` and `_JSON` typing
* Bumped `microlens` dep max

## 2.0.0
* Complete conversion to `microlens`
* All `Prism` are now `Traversal`

## 1.0.0.5
* Fix tests to work against vector-0.11
* Documentation fixes
* No functional changes since 1.0.0.4

## 1.0.0.3
* Move lens upper bound to < 5 like the other packages in the family

## 1
* Module migrated from lens package to Data.Aeson.Lens

## 0.1.2
* Added `members` and `values`

## 0.1.1
* Broadened dependencies

## 0.1
* Repository initialized

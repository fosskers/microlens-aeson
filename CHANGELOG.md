# microlens-aeson

## 2.5.2 (2024-03-01)

#### Changed

- Support for `text-2.1`.

## 2.5.1 (2023-08-16)

#### Changed

- Support for `aeson-2.2`.

## 2.5.0 (2022-03-19)

This is a breaking update that matches upstream changes to `lens-aeson`. Luckily
the changes are mostly simplifications and improvements that better match the
`aeson-2` API. Thanks to `sjshuck` for his contributions to this release.

#### Changed

- Simplify class hierarchy to `AsNumber t => AsValue t`. Change the default
  signature of `_Number` accordingly.
- Move `_String`, `_Bool`, and `_Null` to be methods of class `AsValue`.
- Convert `HashMap Text`-based interfaces to `KeyMap Key`. This changes the
  types of `_Object` and `key`.
- Change `Index Value` to `Key`.
- Require `base >= 4.9`, the same as `aeson-2.*` does.
- Drop dependencies on `deepseq` and `unordered-containers`.

#### Removed

- `Primitive` and class `AsPrimitive`.
- Orphan `Ixed` instances for `HashMap` and `Vector`.

## 2.4.1 (2022-01-21)

#### Added

- `text-2.0` support.

## 2.4.0 (2021-10-21)

#### Changed

- `aeson-2.0` is now the minimum required version.

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

- Restored original `AsJSON` and `_JSON` typing
- Bumped `microlens` dep max

## 2.0.0

- Complete conversion to `microlens`
- All `Prism` are now `Traversal`

## 1.0.0.5

- Fix tests to work against vector-0.11
- Documentation fixes
- No functional changes since 1.0.0.4

## 1.0.0.3

- Move lens upper bound to < 5 like the other packages in the family

## 1

- Module migrated from lens package to Data.Aeson.Lens

## 0.1.2

- Added `members` and `values`

## 0.1.1

- Broadened dependencies

## 0.1

- Repository initialized

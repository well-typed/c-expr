# Revision history for `c-expr`

## 0.2.0.0 -- 2026-10-16

### Breaking changes

- Pointer arithmetic counts elements of the pointee, as in C, and not bytes:
  `p + i` and `i + p` advance `p` by `i` elements, `p - i` moves `p` back by
  `i` elements, and `p1 - p2` counts the elements between `p1` and `p2`. The
  `Add` and `Sub` instances for `Ptr ty` now require the new class
  `Pointee ty`: `Pointee`s are `Storable ty`, whose `sizeOf` is the size of the
  step. In particular, neither `Void` nor `()` is an instance of `Pointee`.
  Arithmetic on `Ptr ()` (`void *`) and on pointers to opaque types is a type
  error, as in C; for byte arithmetic, cast to `Ptr Word8` first. See [issue
  #2306][issue-2306].
- `SPtr` now holds an `SPointee rec ty`: `SVoidPointee` for `void *`, or
  `SObjPointee`, which carries the `Pointee ty` evidence.

### New features

- `C.Type` exports `Pointee`, `NotVoid`, `SPointee(..)`, and `unSPointee`.

### Bug fixes

- `p - i` for an unsigned `i` narrower than `Int` no longer wraps around: on
  64-bit platforms, `p - 1u` moved `p` forward by `UINT_MAX` bytes.

[issue-2306]: https://github.com/well-typed/hs-bindgen/issues/2306

## 0.1.0.0 -- 2026-07-14

- Remove the `C.Char` module; UTF-8 encoding of character and string literals
  now lives in `c-expr-dsl`'s parser.

## 0.1.0-alpha -- 2026-02-06

- Release candidate.

# Revision history for `c-expr`

## 0.2.0.0 -- YYYY-mm-dd

### Breaking changes

- Pointer arithmetic counts elements of the pointee, as in C, and not bytes:
  `p + i`, and `i + p` (`p - i`) advance (move back) `p` by `i` elements, and
  `p1 - p2` counts the elements between `p1` and `p2`.
  The size of the step comes from the `Storable` instance of the pointee, so the
  `Add` and `Sub` instances for `Ptr ty` now require `Storable ty`. Pointers to
  opaque types have no instance, which results in a type error as in C. See
  [issue #2306][issue-2306].
- **Warning:** arithmetic on `Ptr ()` typechecks but is not meaningful, because
  `sizeOf ()` is 0. `p + i`, `i + p` and `p - i` silently return `p` unchanged,
  and `p1 - p2` throws `DivideByZero` at runtime. For byte arithmetic, cast to
  `Ptr Word8` first.
- The function returned by `singAdd` and `singSub` for pointer arguments raises
  an error: lacking the `Storable` instance, it cannot compute the result. The
  result type singleton remains available.

### New features

### Minor changes

### Bug fixes

- `p - i` for an unsigned `i` narrower than `Int` no longer wraps around: on
  64-bit platforms, `p - 1u` moved `p` forward by `UINT_MAX` bytes.

[issue-2306]: https://github.com/well-typed/hs-bindgen/issues/2306

## 0.1.0.0 -- 2026-07-14

- Remove the `C.Char` module; UTF-8 encoding of character and string literals
  now lives in `c-expr-dsl`'s parser.

## 0.1.0-alpha -- 2026-02-06

- Release candidate.

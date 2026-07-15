# `c-expr`

[![License: BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-lightgray.svg)](https://github.com/well-typed/c-expr/blob/main/LICENSE)
[![Build Status](https://github.com/well-typed/c-expr/actions/workflows/haskell.yml/badge.svg)](https://github.com/well-typed/c-expr/actions)

In order to translate C macros to [Haskell][] code, [`hs-bindgen`][] needs to
imbue C macros with a semantics. This project `c-expr` provides one such
semantics.

- The semantics of expressions is based on **C arithmetic expressions**,
  implementing the integral-promotion and arithmetic-conversion rules of the C
  standard.

- The semantics of types is based on `typedef`s.

This project `c-expr` is the main macro language used by [`hs-bindgen`][], but
can be used independently.

`c-expr-dsl` requires an [LLVM/Clang][] installation, as it parses macros via
[`libclang`][LLVM/Clang]; see [`libclang-bindings`][] for setup details.

[Haskell]: https://www.haskell.org/
[`hs-bindgen`]: https://github.com/well-typed/hs-bindgen
[LLVM/Clang]: https://github.com/llvm/llvm-project
[`libclang-bindings`]: https://github.com/well-typed/libclang-bindings/blob/main/manual/README.md

## Packages in this repository

See Section 5.2 `c-expr`: a DSL for C Expressions in Cardwell, Derbyshire & de
Vries, and Schrempf (2025) Automatic C Bindings Generation for Haskell
([DOI](https://dl.acm.org/doi/epdf/10.1145/3759164.3759350),
[PDF](https://well-typed.com/blog/aux/files/haskell2025-hs-bindgen.pdf)).

* [`c-expr-dsl`](c-expr-dsl): a library that provides a DSL for the expression
  and type language implemented in `c-expr`: a [Parsec][]-based parser turning
  [`libclang`][LLVM/Clang] macro tokens into a syntax tree, and a typechecker.

* [`c-expr-runtime`](c-expr-runtime): a library that provides runtime support
  for this DSL. It is the semantic core: a type-level universe of C types and a
  class-per-operator hierarchy whose associated type families encode C's
  conversion rules. For example, addition is defined with the following type
  class, where the result type `AddRes` is computed according to the C
  standard's arithmetic-conversion rules:

  ```haskell
  infixl 2 +
  type Add :: Type -> Type -> Constraint
  class Add a b where
    type family AddRes a b :: Type
    (+) :: a -> b -> AddRes a b
  ```

  Platform-specific instances (e.g., `AddRes CInt CFloat = CDouble`) are
  generated via Template Haskell.

[Parsec]: https://hackage.haskell.org/package/parsec

## Contribution

Our thanks go to those who have contributed to this project with development,
bug reports, feature requests, blog posts, etc.  We list
[contributors](https://github.com/well-typed/hs-bindgen#contributors)
in the `hs-bindgen` README.

Please see [`CONTRIBUTING.md`](CONTRIBUTING.md) for information about
contributing to this project.

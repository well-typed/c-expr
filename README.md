# `c-expr`

[![License: BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-lightgray.svg)](https://github.com/well-typed/c-expr/blob/main/LICENSE)
[![Build Status](https://github.com/well-typed/c-expr/actions/workflows/haskell.yml/badge.svg)](https://github.com/well-typed/c-expr/actions)

`c-expr` is a [Haskell][] project for modeling **C arithmetic expressions** (as
found in C preprocessor macros), implementing the integral-promotion and
arithmetic-conversion rules of the C standard.  It supports the [`hs-bindgen`][]
project, into which it is vendored, but can be used independently.

> [!WARNING]
> This project has not had an official release yet and we are soliciting
> feedback prior to the first official release of [`hs-bindgen`][]. Please try
> it out! If something breaks, please check the [issues][] to see if the problem
> is already known, and open an issue if not.

[Haskell]: https://www.haskell.org/
[issues]: https://github.com/well-typed/c-expr/issues
[`hs-bindgen`]: https://github.com/well-typed/hs-bindgen

## Packages in this repository

* [`c-expr-runtime`](c-expr-runtime), a library that provides a Haskell DSL for
  simple C arithmetic expressions.  It is the semantic core: a type-level
  universe of C types and a class-per-operator hierarchy whose associated type
  families encode C's conversion rules.  For example, addition is defined with
  the following type class, where the result type `AddRes` is computed according
  to the C standard's arithmetic-conversion rules:

  ```haskell
  infixl 2 +
  type Add :: Type -> Type -> Constraint
  class Add a b where
    type family AddRes a b :: Type
    (+) :: a -> b -> AddRes a b
  ```

  Platform-specific instances (e.g. `AddRes CInt CFloat = CDouble`) are
  generated via Template Haskell.

* [`c-expr-dsl`](c-expr-dsl), a library that provides the front end for the
  language supported by `c-expr-runtime`: a [Parsec][]-based parser turning
  [`libclang`][LLVM/Clang] macro tokens into a syntax tree, and a bidirectional
  typechecker targeting the type universe and operator semantics of
  `c-expr-runtime`.

[Parsec]: https://hackage.haskell.org/package/parsec

## Supporting packages

* [`libclang-bindings`](https://github.com/well-typed/libclang-bindings), a
  library that provides bindings for the [LLVM/Clang][] `libclang` C API

[LLVM/Clang]: https://github.com/llvm/llvm-project

## Contribution

Our thanks go to those who have contributed to this project with development,
bug reports, feature requests, blog posts, etc.  We list
[contributors](https://github.com/well-typed/hs-bindgen#contributors)
in the `hs-bindgen` README.

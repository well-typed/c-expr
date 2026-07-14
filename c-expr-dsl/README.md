# `c-expr-dsl`

`c-expr-dsl` is a [Haskell][] library providing a DSL for the C expression and
type language implemented by [`c-expr-runtime`][]: a [Parsec][]-based parser
turning [LLVM/Clang][] `libclang` macro tokens into a syntax tree, and a
bidirectional typechecker. It supports the [`hs-bindgen`][] project but can be
used independently.

`c-expr-dsl` requires an LLVM/Clang installation, as it parses macros via
`libclang`; see [`libclang-bindings`][] for setup details.

See the [main README][] for more information, and the [changelog][] for
release notes.

[Haskell]: <https://www.haskell.org/>
[Parsec]: <https://hackage.haskell.org/package/parsec>
[LLVM/Clang]: <https://github.com/llvm/llvm-project>
[`c-expr-runtime`]: <https://github.com/well-typed/c-expr/tree/main/c-expr-runtime>
[`hs-bindgen`]: <https://github.com/well-typed/hs-bindgen>
[`libclang-bindings`]: <https://github.com/well-typed/libclang-bindings/blob/main/manual/README.md>
[main README]: <https://github.com/well-typed/c-expr#readme>
[changelog]: <https://github.com/well-typed/c-expr/blob/main/c-expr-dsl/CHANGELOG.md>

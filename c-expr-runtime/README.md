# `c-expr-runtime`

`c-expr-runtime` is a [Haskell][] library providing the runtime support for
[`c-expr-dsl`][]: a type-level universe of C types and a class-per-operator
hierarchy whose associated type families encode the C standard's
integral-promotion and arithmetic-conversion rules. It supports the
[`hs-bindgen`][] project but can be used independently.

Its test suite cross-checks operator result types against a real C compiler,
and so requires an [LLVM/Clang][] installation.

See the [main README][] for more information, and the [changelog][] for
release notes.

[Haskell]: <https://www.haskell.org/>
[LLVM/Clang]: <https://github.com/llvm/llvm-project>
[`c-expr-dsl`]: <https://github.com/well-typed/c-expr/tree/main/c-expr-dsl>
[`hs-bindgen`]: <https://github.com/well-typed/hs-bindgen>
[main README]: <https://github.com/well-typed/c-expr#readme>
[changelog]: <https://github.com/well-typed/c-expr/blob/main/c-expr-runtime/CHANGELOG.md>

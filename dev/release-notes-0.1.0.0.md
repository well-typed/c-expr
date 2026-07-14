# Release 0.1.0.0

First non-alpha release of `c-expr-runtime` and `c-expr-dsl`, the two packages
that model C arithmetic-expression semantics for `hs-bindgen`'s macro
translation.

## `c-expr-runtime`

### Breaking changes

* Remove `C.Char`; UTF-8 encoding of char/string literals now lives in
  `c-expr-dsl`'s parser.

## `c-expr-dsl`

### Breaking changes

* Rename `Checked{Type,Value}Expr` to `Typechecked{Type,Value}Expr`, adding
  `Functor`/`Foldable`/`Traversable` instances; rename `TypeSource` to
  `CTypeSource` (`TypeSourceTypedef`/`TypeSourceMacroType` become
  `FromTypedef`/`FromMacroType`).
* Re-export parse/typecheck APIs from `C.Expr.Parse` / `C.Expr.Typecheck`;
  demote lower-level modules to `other-modules`.
* `Expr`/`Term` gain a de Bruijn `ctx :: Ctx` index for the macro parameter
  scope (`macroArgs :: [Name]` → `macroParams :: Vec ctx Name`); `sameMacro`
  now compares structurally, then is removed
  ([hs-bindgen#1983](https://github.com/well-typed/hs-bindgen/pull/1983)).
* Rename `Name` to `Identifier`; the new `Name` type distinguishes ordinary
  names from tagged-type names, and tagged types now parse as `Var` nodes
  instead of a `TypeTagged` literal.
* `CharLiteral.charLiteralValue` is now `CChar`; `StringLiteral.stringLiteralValue`
  is now a strict UTF-8 `ByteString`.
* `tcMacros` takes a single `ann -> Maybe QuantTy` projection instead of a
  typedef set and injection callbacks; `CTypeSource`, `buildTypedefEnv`, and
  `MacroTcInjectError` are removed.

### New features

* Parse macro types as well as expressions, deferring the type-vs-value
  distinction to typechecking
  ([hs-bindgen#1862](https://github.com/well-typed/hs-bindgen/pull/1862)).
* Resolve local macro parameters to de Bruijn indices at parse time.
* Reject type-like macros expanding to an incomplete type (`void`/`const void`)
  with a new `TcIncompleteTypeMacro` error.
* Support multi-line macro definitions
  ([hs-bindgen#1993](https://github.com/well-typed/hs-bindgen/pull/1993)).
* New parser/typechecker test suite, including full char/string literal
  coverage.

### Bug fixes

* Only parse macros as function-like when there's no whitespace before `(`,
  per the C reference
  ([hs-bindgen#1990](https://github.com/well-typed/hs-bindgen/pull/1990)).

---

**Full changelogs**:
[`c-expr-runtime`](https://github.com/well-typed/c-expr/blob/release-0.1.0.0/c-expr-runtime/CHANGELOG.md) ·
[`c-expr-dsl`](https://github.com/well-typed/c-expr/blob/release-0.1.0.0/c-expr-dsl/CHANGELOG.md)

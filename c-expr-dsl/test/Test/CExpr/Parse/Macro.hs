-- | Unit tests for 'C.Expr.Parse.parseMacroBody'
--
-- Tests the macro body parser, focusing on:
--
-- * Type bodies vs expression bodies (disambiguation)
-- * Bodies of object-like and function-like macros
--
-- Splitting a definition into name, formal parameters and body is the
-- embedder's job, so the parameters are an input here rather than something
-- the parser recovers from the token stream.
module Test.CExpr.Parse.Macro (tests) where

import Data.Either (isLeft, isRight)
import Data.Vec.Lazy (Vec (..))
import DeBruijn (Idx (..), pattern I1)
import Test.Tasty
import Test.Tasty.HUnit

import C.Expr.Syntax

import Clang.CStandard

import Test.CExpr.Parse.Infra
import Test.CExpr.Typecheck.Infra (add, intLit, mtagged, mtuple, mvar)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Parse.Macro" [
      testsWithCStd cStd | cStd <- [minBound .. maxBound :: CStandard]
    ]

testsWithCStd :: CStandard -> TestTree
testsWithCStd cStd = testGroup (show cStd) [
      testGroup "type bodies"               $ tests_typeBody         std
    , testGroup "function-like type bodies" $ tests_funcLikeTypeBody std
    , testGroup "expression bodies"         $ tests_exprBody         std
    , testGroup "comma bodies"              $ tests_commaBody        std
    , testGroup "disambiguation"            $ tests_disambiguation   std
    ]
  where
    std = ClangCStandard cStd DisableGnu

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- | True when the macro expression looks like a type: it has an 'Type' or
-- 'TyApp' at its core (bare identifier cases are intentionally excluded
-- because a bare name is structurally identical in both type and expression
-- position after the refactor).
isTypeBody :: Either e (Expr ctx (Ps ann)) -> Bool
isTypeBody (Right body) = case body of
    Term (Literal (TypeLit _)) -> True
    TyApp {}                   -> True
    _                          -> False
isTypeBody _ = False

-- | True when the macro expression is unambiguously an expression (a literal
-- or an operator application), not a type.
isExprBody :: Either e (Expr ctx (Ps ann)) -> Bool
isExprBody (Right body) = case body of
    Term (Literal (ValueLit (ValueInt _)))    -> True
    Term (Literal (ValueLit (ValueFloat _)))  -> True
    Term (Literal (ValueLit (ValueChar _)))   -> True
    Term (Literal (ValueLit (ValueString _))) -> True
    VaApp {}                                  -> True
    _                                         -> False
isExprBody _ = False

{-------------------------------------------------------------------------------
  Type bodies
-------------------------------------------------------------------------------}

tests_typeBody :: ClangCStandard -> [TestTree]
tests_typeBody cStd = [
      testCase "int" $
        -- #define FOO int
        checkBody cStd VNil [kw "int"]
          @?= Right (tyLit (TypeInt Nothing (Just SizeInt)))
    , testCase "unsigned long" $
        -- #define FOO unsigned long
        checkBody cStd VNil [kw "unsigned", kw "long"]
          @?= Right (tyLit (TypeInt (Just Unsigned) (Just SizeLong)))
    , testCase "const int*" $
        -- #define FOO const int *
        checkBody cStd VNil [kw "const", kw "int", punc "*"]
          @?= Right (TyApp Pointer (TyApp Const (tyLit (TypeInt Nothing (Just SizeInt)) ::: VNil) ::: VNil))
    , testCase "void*" $
        -- #define FOO void *
        checkBody cStd VNil [kw "void", punc "*"]
          @?= Right (TyApp Pointer (tyLit TypeVoid ::: VNil))
    , testCase "struct Foo" $
        -- #define FOO struct Foo
        checkBody cStd VNil [kw "struct", ident "Foo"]
          @?= Right (mtagged "Foo" TagStruct)
    , testCase "size_t" $
        -- #define FOO size_t (bare identifier; typechecker decides it's a type)
        checkBody cStd VNil [ident "size_t"]
          @?= Right (mvar "size_t")
    , testCase "_Bool" $
        -- #define FOO _Bool
        checkBody cStd VNil [kw "_Bool"]
          @?= Right (tyLit TypeBool)
    , testCase "size_t const * const" $
        -- #define FOO size_t const * const
        checkBody cStd VNil [ident "size_t", kw "const", punc "*", kw "const"]
          @?= Right (TyApp Const (TyApp Pointer (TyApp Const (mvar "size_t" ::: VNil) ::: VNil) ::: VNil))
    ]

{-------------------------------------------------------------------------------
  Function-like type bodies (local args)
-------------------------------------------------------------------------------}

tests_funcLikeTypeBody :: ClangCStandard -> [TestTree]
tests_funcLikeTypeBody cStd = [
      testCase "PTR(T) = T*" $
        -- #define PTR(T) T*
        -- T is a local arg; the body is a pointer type parameterised by T.
        checkBody cStd (Identifier "T" ::: VNil) [ident "T", punc "*"]
          @?= Right (TyApp Pointer (Term (LocalParam IZ) ::: VNil))
    , testCase "CONST_PTR(T) = const T*" $
        -- #define CONST_PTR(T) const T*
        checkBody cStd (Identifier "T" ::: VNil) [kw "const", ident "T", punc "*"]
          @?= Right (TyApp Pointer (TyApp Const (Term (LocalParam IZ) ::: VNil) ::: VNil))
    , testCase "free var is not a local arg" $
        -- #define PTR(T) size_t*
        -- size_t is not a formal parameter, so it stays as Var, not LocalParam.
        checkBody cStd (Identifier "T" ::: VNil) [ident "size_t", punc "*"]
          @?= Right (TyApp Pointer (mvar "size_t" ::: VNil))
    ]

{-------------------------------------------------------------------------------
  Expression bodies
-------------------------------------------------------------------------------}

tests_exprBody :: ClangCStandard -> [TestTree]
tests_exprBody cStd = [
      -- Object-like macros
      testCase "integer literal" $
        -- #define FOO 42
        assertBool "expected expression body" $
          isExprBody (checkBody cStd VNil [lit "42"])
    , testCase "negative literal" $
        -- #define FOO -1
        assertBool "expected expression body" $
          isExprBody (checkBody cStd VNil [punc "-", lit "1"])
    , testCase "arithmetic expression" $
        -- #define FOO 1 + 2
        assertBool "expected expression body" $
          isExprBody (checkBody cStd VNil [lit "1", punc "+", lit "2"])
      -- Function-like macros
      -- A bare identifier body (e.g. x) is structurally identical for type and
      -- expression positions after the Expr unification; we just check it parses.
    , testCase "identity function" $
        -- #define FOO(x) x
        assertBool "expected parse success" $
          isRight $ checkBody cStd (Identifier "x" ::: VNil) [ident "x"]
    , testCase "two-argument function" $
        -- #define FOO(a, b) a + b
        -- Parameters are given in source order; the /last/ one is the innermost
        -- binder, so a is I1 and b is IZ.
        checkBody cStd (Identifier "a" ::: Identifier "b" ::: VNil)
            [ident "a", punc "+", ident "b"]
          @?= Right (add (Term (LocalParam I1)) (Term (LocalParam IZ)))
    , testCase "zero-argument function" $
        -- #define FOO() 0
        assertBool "expected expression body" $
          isExprBody (checkBody cStd VNil [lit "0"])
    ]

{-------------------------------------------------------------------------------
  Comma bodies

  A comma in a macro body denotes a tuple, not the C comma operator; see
  <https://github.com/well-typed/hs-bindgen/issues/2182>.
-------------------------------------------------------------------------------}

tests_commaBody :: ClangCStandard -> [TestTree]
tests_commaBody cStd = [
      testCase "(1, 2)" $
        -- #define FOO (1, 2)
        checkBody cStd VNil [punc "(", lit "1", punc ",", lit "2", punc ")"]
          @?= Right (mtuple (intLit 1 ::: intLit 2 ::: VNil))
    , testCase "1, 2 (without parentheses)" $
        -- #define FOO 1, 2
        checkBody cStd VNil [lit "1", punc ",", lit "2"]
          @?= Right (mtuple (intLit 1 ::: intLit 2 ::: VNil))
    , testCase "(1, 2, 3)" $
        -- #define FOO (1, 2, 3)
        checkBody cStd VNil
            [punc "(", lit "1", punc ",", lit "2", punc ",", lit "3", punc ")"]
          @?= Right (mtuple (intLit 1 ::: intLit 2 ::: intLit 3 ::: VNil))
    , testCase "components are full expressions" $
        -- #define FOO (1 + 2, 3)
        checkBody cStd VNil
            [punc "(", lit "1", punc "+", lit "2", punc ",", lit "3", punc ")"]
          @?= Right (mtuple (add (intLit 1) (intLit 2) ::: intLit 3 ::: VNil))
    , testCase "FOO(x, y) = (x, y)" $
        -- #define FOO(x, y) (x, y)
        checkBody cStd (Identifier "x" ::: Identifier "y" ::: VNil)
            [punc "(", ident "x", punc ",", ident "y", punc ")"]
          @?= Right (mtuple (Term (LocalParam I1) ::: Term (LocalParam IZ) ::: VNil))
    , testCase "FOO(x, y) = ((x), (y))" $
        -- #define FOO(x, y) ((x), (y))
        checkBody cStd (Identifier "x" ::: Identifier "y" ::: VNil)
            [ punc "(", punc "(", ident "x", punc ")", punc ","
            ,           punc "(", ident "y", punc ")", punc ")"
            ]
          @?= Right (mtuple (Term (LocalParam I1) ::: Term (LocalParam IZ) ::: VNil))
    ]

{-------------------------------------------------------------------------------
  Disambiguation: types vs. expressions
-------------------------------------------------------------------------------}

tests_disambiguation :: ClangCStandard -> [TestTree]
tests_disambiguation cStd = [
      -- A bare identifier like 'size_t' is now structurally identical whether
      -- it came from the type parser or the expression parser (both produce
      -- Term (Var ...)).  We just verify that parsing succeeds.
      testCase "bare name parses successfully" $
        -- #define FOO size_t
        assertBool "expected parse success" $
          isRight (checkBody cStd VNil [ident "size_t"])
    , testCase "void is a type body, not an identifier expression" $
        -- #define FOO void
        assertBool "expected type body" $
          isTypeBody (checkBody cStd VNil [kw "void"])
      -- An integer literal cannot be a type, so it falls through to expression.
    , testCase "literal falls through to expression" $
        -- #define FOO 0
        assertBool "expected expression body" $
          isExprBody (checkBody cStd VNil [lit "0"])
      -- A parenthesised literal is not a type either.
    , testCase "parenthesised expression is not a type" $
        -- #define FOO (1)
        assertBool "expected expression body" $
          isExprBody (checkBody cStd VNil [punc "(", lit "1", punc ")"])
      -- Completely unparseable input
    , testCase "bare comma fails" $
        -- #define FOO ,
        assertBool "expected failure" $
          isLeft (checkBody cStd VNil [punc ","])
    , testCase "empty body fails" $
        -- #define FOO
        assertBool "expected failure" $
          isLeft (checkBody cStd VNil [])
    ]

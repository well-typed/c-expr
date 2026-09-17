// Fixture for c-expr-dsl golden tests.
//
// Each macro is tokenised using libclang, split into its formal parameters and
// its body, and then fed to 'parseMacroBody'. The results are compared against
// the golden file macros.golden.

// ---------------------------------------------------------------------------
// Type macros: void and bool
// ---------------------------------------------------------------------------

#define TY_VOID void
#define TY_BOOL _Bool
#define TY_BOOL_C23 bool

// ---------------------------------------------------------------------------
// Type macros: integer types
// ---------------------------------------------------------------------------

#define TY_INT int
#define TY_SIGNED signed
#define TY_UNSIGNED unsigned
#define TY_SHORT short
#define TY_LONG long
#define TY_LONG_LONG long long
#define TY_UNSIGNED_INT unsigned int
#define TY_SIGNED_INT signed int
#define TY_UNSIGNED_SHORT unsigned short
#define TY_UNSIGNED_LONG unsigned long
#define TY_UNSIGNED_LONG_LONG unsigned long long
#define TY_LONG_LONG_INT long long int

// ---------------------------------------------------------------------------
// Type macros: char
// ---------------------------------------------------------------------------

#define TY_CHAR char
#define TY_SIGNED_CHAR signed char
#define TY_UNSIGNED_CHAR unsigned char

// ---------------------------------------------------------------------------
// Type macros: floating-point
// ---------------------------------------------------------------------------

#define TY_FLOAT float
#define TY_DOUBLE double

// ---------------------------------------------------------------------------
// Type macros: const qualifiers
// ---------------------------------------------------------------------------

#define TY_CONST_INT const int
#define TY_INT_CONST int const
#define TY_CONST_VOID const void

// ---------------------------------------------------------------------------
// Type macros: pointer types
// ---------------------------------------------------------------------------

#define TY_INT_PTR int *
#define TY_INT_PTR_PTR int **
#define TY_VOID_PTR void *
#define TY_CONST_INT_PTR const int *
#define TY_INT_PTR_CONST int *const

// ---------------------------------------------------------------------------
// Type macros: mixed const pointer types
// ---------------------------------------------------------------------------

#define PTR_TO_CONST_L const int *
#define PTR_TO_CONST_R int const *
#define CONST_PTR int *const
#define CONST_PTR_TO_CONST_L const int *const
#define CONST_PTR_TO_CONST_R int const *const
#define CONST_PTR_CHAIN_1 int const *const *const *
#define CONST_PTR_CHAIN_2 int const *const *const *const
#define CONST_PTR_CHAIN_3 int const *const **const *const
#define CONST_PTR_CHAIN_4 const int *const *const *
#define CONST_PTR_CHAIN_5 const int *const *const *const
#define CONST_PTR_CHAIN_6 const int *const **const *const
#define CONST_PTR_CHAIN_7 const int ****const
#define CONST_PTR_CHAIN_8 int const ****const
#define CONST_PTR_CHAIN_9 int ****const *

// ---------------------------------------------------------------------------
// Type macros: tagged types (struct / union / enum)
// ---------------------------------------------------------------------------

#define TY_STRUCT_FOO struct Foo
#define TY_UNION_BAR union Bar
#define TY_ENUM_BAZ enum Baz
#define TY_STRUCT_FOO_PTR struct Foo *

// ---------------------------------------------------------------------------
// Type macros: named types (bare identifiers treated as typedef names)
//
// No #include is needed: libclang tokenises these as CXToken_Identifier
// regardless of whether the name is declared, which is exactly what
// 'parseMacroType' expects.
//
// This also covers bare macro-name references: a single identifier produces
// Term (Var ...) regardless of which parser handles it, because both the
// type parser and the expression parser parse a bare identifier the same way.
// To get a macro-name reference in expression position with arithmetic, use
// an expression context (e.g. EXPR_ONE + EXPR_FORTY_TWO).
// ---------------------------------------------------------------------------

#define TY_SIZE_T size_t
#define TY_UINT32_T uint32_t
#define TY_CONST_SIZE_T const size_t
#define TY_SIZE_T_PTR size_t *
// A bare macro name parses as a named type, not an expression variable
#define TY_MACRO_REF EXPR_FORTY_TWO

// ---------------------------------------------------------------------------
// Expression macros: integer literals
// ---------------------------------------------------------------------------

#define EXPR_ZERO 0
#define EXPR_ONE 1
#define EXPR_FORTY_TWO 42
#define EXPR_HEX 0xFF
#define EXPR_NEG (-1)

// ---------------------------------------------------------------------------
// Expression macros: arithmetic
// ---------------------------------------------------------------------------

#define EXPR_ADD 1 + 2
#define EXPR_SUB 10 - 3
#define EXPR_MUL 4 * 5
#define EXPR_DIV 8 / 2
#define EXPR_SHIFT_LEFT 1 << 4
#define EXPR_BITWISE_OR 0x0F | 0xF0

// ---------------------------------------------------------------------------
// Expression macros: parenthesised / compound
// ---------------------------------------------------------------------------

#define EXPR_PARENS (42)
#define EXPR_COMPOUND (1 + 2) * 3

// ---------------------------------------------------------------------------
// Expression macros: comma
//
// A comma in a macro body denotes a tuple, not the C comma operator.
//
// C assigns no meaning to macro bodies, so we are free to pick the most useful
// interpretation. Tuples support the argument-list idiom (`#define ARGS 1, 2`
// used as `f(ARGS)`).
//
// The comma operator resembles side effects, which we cannot express in Haskell
// in a meaningful way anyways.
//
// See <https://github.com/well-typed/hs-bindgen/issues/2182>.
// ---------------------------------------------------------------------------

#define EXPR_TUPLE (1, 2)
#define EXPR_TUPLE_NO_PARENS 1, 2
#define EXPR_TUPLE_THREE (1, 2, 3)

// ---------------------------------------------------------------------------
// Function-like macros
// ---------------------------------------------------------------------------

#define FUNC_IDENTITY(x) x
#define FUNC_ADD(a, b) a + b
#define FUNC_NEG(x) (-x)
#define FUNC_MULTIPLE_LOCAL_PARAMS(a, b, c, d) a + (b - (c + d))
// Comma is a tuple here, too (see EXPR_TUPLE above)
#define FUNC_TUPLE(x, y) (x, y)
#define FUNC_TUPLE_THREE(x, y, z) ((x), (y), (z))
// TODO <https://github.com/well-typed/c-expr/issues/1>
//
// Ternary operator is not yet in the expression grammar (see below).
// #define FUNC_MAX(a, b)       ((a) > (b) ? (a) : (b))

// FUNC_MULTILINE is like FUNC_SINGLELINE, but the former's definition is spread
// over multiple lines. Both macros should lead to the same parse result.
#define FUNC_SINGLELINE(xx, yy) xx * yy
#define FUN\
C_MULTIL\
\
INE\
\
\
(\
xx\
,\
yy\
)\
x\
x * y\
y

// ---------------------------------------------------------------------------
// Function-like macros: parameters spelled like keywords
//
// The preprocessor works on pp-tokens, which have no keywords, so these are
// valid C in every standard, and inside the replacement list the parameter
// shadows the keyword. libclang classifies the spelling according to the
// language options of the translation unit ('bool' is CXToken_Identifier under
// -std=c17 and CXToken_Keyword under -std=c2x), so every parameter reference
// below must come out identical in both golden files.
// ---------------------------------------------------------------------------

#define KWPARAM_BOOL(bool) bool
#define KWPARAM_BOOL_PTR(bool) bool *
#define KWPARAM_INT(int) int
#define KWPARAM_CONST(const) const
#define KWPARAM_SIZEOF(sizeof) sizeof + 1
#define KWPARAM_CONST_PTR(const) const *
// The parameter is unused, so the body is an ordinary free variable
#define KWPARAM_UNUSED(int) x
// The shadowing also holds in qualifier, specifier and tag position. None of
// these bodies is representable (a qualifier or specifier applied to a
// parameter has no syntax tree), so all must be rejected rather than parsed
// with the parameter silently dropped.
#define KWPARAM_QUALIFIER(const) const x
#define KWPARAM_SPECIFIER(int) unsigned int
#define KWPARAM_TAG(Foo) struct Foo
// 'bool' is not a parameter here, so it keeps its keyword meaning: the type
// bool under C23, a named type under C17
#define KWPARAM_SHADOWS_NOTHING(x) bool

// ---------------------------------------------------------------------------
// Expression macros: references to other macros / typedefs
//
// Macro names and typedef names are tokenised as CXToken_Identifier, so
// the parser treats them as variable references (Var).  The expression
// parser also supports call syntax: FOO(x) parses as Var "FOO" [x].
// ---------------------------------------------------------------------------

// Arithmetic that combines two macro-name references
#define EXPR_REF_ADD EXPR_ONE + EXPR_FORTY_TWO

// Call a previously defined function-like macro with literal arguments
#define EXPR_CALL_ADD FUNC_ADD(1, 2)

// Call a previously defined function-like macro inside a larger expression
#define EXPR_CALL_NESTED FUNC_ADD(EXPR_ONE, EXPR_FORTY_TWO) + 1

// ---------------------------------------------------------------------------
// Erroneous macros or macros with bodies the parser cannot handle
// ---------------------------------------------------------------------------

// A macro is only function-like if there is no whitespace between the macro
// name and the opening parenthesis of the parameter list.
//
// Beware: casts look like function-like macros, but they are parsed as
// object-like macros because there is whitespace after the macro name.
#define CAST_SINGLE_NOKW (X) x
#define CAST_SINGLE_KW (int)x
#define CAST_MULTI_KW (unsigned int)x

// We can parse this macro, but typecheck will fail.
#define TYPE_FUN_WITH_PARAM(X) int

// TODO <https://github.com/well-typed/c-expr/issues/1>
//
// Ternary operator is not yet in the expression grammar.
#define BAD_TERNARY a ? b : c

// 'long double' is not supported (TypeFloat only covers float/double).
#define BAD_LONG_DOUBLE long double

// '_Pragma' is the C99 preprocessor operator, equivalent to '#pragma'. It
// expands to a preprocessing directive, not a C expression, so we reject it.
#define PACK_START _Pragma("pack(1)")
#define PACK_FINISH _Pragma("pack()")

// ---------------------------------------------------------------------------
// A macro name spelled like a keyword
//
// Also valid C. Defined last, because from here on the spelling is a macro.
// ---------------------------------------------------------------------------

#define bool int

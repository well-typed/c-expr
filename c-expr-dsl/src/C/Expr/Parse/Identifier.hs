-- | Parsing C identifiers
module C.Expr.Parse.Identifier (
    parseIdentifier
  , parseLocIdentifier
  ) where

import Control.Monad

import C.Expr.Parse.Infra
import C.Expr.Syntax.Identifier

import Clang.Enum.Simple
import Clang.HighLevel.Types
import Clang.LowLevel.Core

{-------------------------------------------------------------------------------
  Identifiers
-------------------------------------------------------------------------------}

-- | Parse an identifier
--
-- Does not accept C keywords. Use 'parseLocIdentifier' when the token may be a
-- keyword (e.g. for macro names, where @#define bool int@ is valid C).
parseIdentifier :: Parser Identifier
parseIdentifier = token $ \t -> do
    let spelling = getTokenSpelling (tokenSpelling t)
    let ki = fromSimpleEnum (tokenKind t)
    guard $ ki == Right CXToken_Identifier
    return $ Identifier spelling

-- | Parse an identifier together with its source location
--
-- Accepts both identifiers and keywords. In later LLVMs (not in 14, surely in
-- 16), @bool@ is classified as a keyword rather than an identifier. We accept
-- keywords here so that macros such as @#define bool int@ can be parsed. Even
-- in C23 the meaning of @bool@ can be overwritten (the macro takes precedence).
parseLocIdentifier :: Parser (Range MultiLoc, Identifier)
parseLocIdentifier = token $ \t -> do
    let spelling = getTokenSpelling (tokenSpelling t)
    let ki = fromSimpleEnum (tokenKind t)
    guard $ ki == Right CXToken_Identifier || ki == Right CXToken_Keyword
    return (
        tokenExtent t
      , Identifier spelling
      )

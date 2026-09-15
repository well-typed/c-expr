-- | Parsing C identifiers
module C.Expr.Parse.Identifier (
    parseIdentifier
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
-- Does not accept C keywords.
parseIdentifier :: Parser Identifier
parseIdentifier = token $ \t -> do
    let spelling = getTokenSpelling (tokenSpelling t)
    let ki = fromSimpleEnum (tokenKind t)
    guard $ ki == Right CXToken_Identifier
    return $ Identifier spelling

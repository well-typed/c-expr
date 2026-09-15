module C.Expr.Parse (
    -- * Parsing macro bodies
    parseMacroBody
  , parseMacroType
    -- * Parser infrastructure
  , Parser
  , runParser
  , MacroParseError(..)
  ) where

import C.Expr.Parse.Expr (parseMacroBody, parseMacroType)
import C.Expr.Parse.Infra (MacroParseError (..), Parser, runParser)

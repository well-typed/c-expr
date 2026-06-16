module C.Expr.Util.Panic (
    panicPure
  , panicIO
  ) where

import Control.Exception
import Control.Monad.IO.Class
import GHC.Stack

-- | Unexpected (e.g. invariant violation) conditions.
data PanicException = PanicException !CallStack !String
  deriving Show

instance Exception PanicException where
    displayException (PanicException cs  msg) = unlines
        [ "PANIC!: the impossible happened"
        , pleaseReport
        , msg
        , prettyCallStack cs
        ]

pleaseReport :: String
pleaseReport = "Please report this as a bug at https://github.com/well-typed/c-expr/issues/"

-- | Panic in pure context
panicPure :: HasCallStack => String -> a
panicPure msg = throw (PanicException callStack msg)

-- | Panic in IO
panicIO :: (HasCallStack, MonadIO m) => String -> m a
panicIO msg = liftIO (throwIO (PanicException callStack msg))

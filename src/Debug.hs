module Debug
(
    stub,
    dump,
) where

import Debug.Trace
import Development.Placeholders
import Language.Haskell.TH (Q, Exp)

stub :: Q Exp
stub = (placeholderNoWarning "stub impl")

dump ::(Monad m, Show a) => a -> m ()
dump a = traceShow a $ return ()

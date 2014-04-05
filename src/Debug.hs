module Debug
(
    stub
) where

import Development.Placeholders
import Language.Haskell.TH (Q, Exp)

stub :: Q Exp
stub = (placeholderNoWarning "stub impl")

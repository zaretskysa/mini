module Evaluating.ConversionM
(
    toBoolValue,
    toBool
) where

import Evaluating.Eval
import Evaluating.Value
import qualified Evaluating.Conversion as Conv

toBoolValue :: Value -> Eval Value
toBoolValue val = return $ Conv.toBoolValue val

toBool :: Value -> Eval Bool
toBool val = return $ Conv.toBool val

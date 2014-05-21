module Evaluating.ConversionM
(
    toBoolValue,
    toBool,
    toNumberValue,
    toDouble
) where

import Evaluating.Eval
import Evaluating.Value
import qualified Evaluating.Conversion as Conv

toBoolValue :: Value -> Eval Value
toBoolValue val = return $ Conv.toBoolValue val

toBool :: Value -> Eval Bool
toBool val = return $ Conv.toBool val

toNumberValue :: Value -> Eval Value
toNumberValue = return . Conv.toNumberValue

toDouble :: Value -> Eval Double
toDouble = return . Conv.toDouble

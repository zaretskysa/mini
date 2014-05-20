module Evaluating.Conversion
(
    toBoolValue,
    toBool
) where

import Evaluating.Value

toBoolValue :: Value -> Value
toBoolValue (NumberValue num) = BoolValue $ num /= 0
toBoolValue val@(BoolValue _) = val
toBoolValue (FunctionValue _) = BoolValue True
toBoolValue UndefinedValue = BoolValue False

toBool :: Value -> Bool
toBool val = 
    case toBoolValue val of
        BoolValue True -> True
        _ -> False

module Evaluating.Conversion
(
    toBoolValue,
    toBool,
    toNumberValue,
    toDouble
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

toNumberValue :: Value -> Value
toNumberValue = NumberValue . toDouble

toDouble :: Value -> Double
toDouble (NumberValue num) = num
toDouble (BoolValue True) = 1
toDouble (BoolValue False) = 0
toDouble (FunctionValue _) = 0
toDouble UndefinedValue = 0

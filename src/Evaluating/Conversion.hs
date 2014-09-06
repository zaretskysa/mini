module Evaluating.Conversion
(
    toBoolValue,
    toBool,
    toNumberValue,
    toDouble,
) where

import Evaluating.Value

toBoolValue :: Value -> Value
toBoolValue (NumberValue num) = BoolValue $ num /= 0
toBoolValue val@(BoolValue _) = val
toBoolValue (FunctionValue _) = BoolValue True
toBoolValue UndefinedValue = BoolValue False
toBoolValue (RefValue _) = BoolValue True
toBoolValue (StringValue "") = BoolValue False
toBoolValue (StringValue _) = BoolValue True
toBoolValue (ObjectValue _) = BoolValue True

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
toDouble (RefValue _) = 0
toDouble (StringValue _) = 0
toDouble (ObjectValue _) = 0

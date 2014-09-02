module Evaluating.Value where

import Control.Monad.Error

import Parsing.Ast
import Types

type MaybeValue = Maybe Value

type Values = [Value]

data Value =
      NumberValue Double
    | BoolValue Bool
    | StringValue String
    | FunctionValue FunctionDeclaration
    | UndefinedValue
    | RefValue Reference
    deriving (Show, Eq)

instance Error Value where
    noMsg = StringValue "mini_exception"
    strMsg str = StringValue str

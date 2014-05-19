module Evaluating.Value where

import Parsing.Ast

type MaybeValue = Maybe Value

type Values = [Value]

data Value =
      NumberValue Double
    | BoolValue Bool
    | FunctionValue FunctionDeclaration
    | UndefinedValue
    deriving (Show, Eq)

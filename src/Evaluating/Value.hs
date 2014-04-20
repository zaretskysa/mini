module Evaluating.Value where

import Parsing.Ast

type MaybeValue = Maybe Value

data Value =
      NumberValue Double
    | FunctionValue FunctionDeclaration
    deriving (Show, Eq)

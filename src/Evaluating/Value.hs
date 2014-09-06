module Evaluating.Value where

import Control.Monad.Error

import Parsing.Ast
import Types
import {-# SOURCE #-} Evaluating.Object (Object)

type MaybeValue = Maybe Value

type Values = [Value]

data Value =
      UndefinedValue
    | BoolValue Bool
    | NumberValue Double
    | StringValue String
    | FunctionValue FunctionDeclaration
    | ObjectValue Object
    | RefValue Reference
    deriving (Show, Eq)

instance Error Value where
    noMsg = StringValue "mini_exception"
    strMsg str = StringValue str

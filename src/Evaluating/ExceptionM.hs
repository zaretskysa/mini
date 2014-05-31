module Evaluating.ExceptionM
(
    throw,
    throwNoBinding,
    throwNotFunction,
) where

import Control.Monad.Error

import Types
import Evaluating.Eval
import Evaluating.Value

throw :: Value -> Eval Value
throw ex = throwError ex

throwNoBinding :: Identifier -> Eval Value
throwNoBinding identifier = throw $ StringValue $ "no_binding_" ++ identifier

throwNotFunction :: Identifier -> Eval Value
throwNotFunction identifier = throw $ StringValue $ "not_function_" ++ identifier

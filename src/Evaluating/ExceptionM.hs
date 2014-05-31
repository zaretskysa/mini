module Evaluating.ExceptionM
(
    Exception(..),

    throw,
    throwNoBinding,
    throwNotFunction,
    throwValue,
) where

import Control.Monad.Error

import Types
import Evaluating.Eval
import Evaluating.Exception
import Evaluating.Value


throw :: Exception -> Eval Value
throw ex = throwError ex

throwNoBinding :: Identifier -> Eval Value
throwNoBinding identifier = throw $ NoBindingException identifier

throwNotFunction :: Identifier -> Eval Value
throwNotFunction identifier = throw $ NotFunctionException identifier

throwValue :: Value -> Eval Value
throwValue value = throw $ ValueException value

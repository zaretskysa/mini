module Evaluating.Exception
(
    Exception(..)
) where

import Control.Monad.Error

import Types
import Evaluating.Value

data Exception = 
      NoBindingException Identifier
    | NotFunctionException Identifier
    | StubException
    | ValueException Value
    deriving(Show, Eq)

instance Error Exception where
    noMsg = StubException
    strMsg str = StubException

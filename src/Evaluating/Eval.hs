module Evaluating.Eval
(
    module Control.Monad.Identity,
    module Control.Monad.State,

    Eval,

    runEval,
) where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Cont
import Control.Monad.Error

import Evaluating.Value
import Evaluating.Environment (Environment, empty)

type Eval a = ErrorT Value (ContT (Either Value Value) (StateT Environment Identity)) a

runEval :: Eval Value -> (Either Value Value, Environment)
runEval eval = runIdentity (runStateT (runContT (runErrorT eval) return) empty)

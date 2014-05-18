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

import Evaluating.Value
import Evaluating.Environment (Environment, empty)

type Eval a = ContT Value (StateT Environment Identity) a

runEval :: Eval Value -> (Value, Environment)
runEval eval = runIdentity (runStateT (runContT eval return) empty)

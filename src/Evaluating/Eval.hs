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

type Eval a = StateT Environment (ContT Value Identity) a

runEval :: Eval Value -> Value
runEval eval = runIdentity (runContT (runStateT eval empty) (return . fst))

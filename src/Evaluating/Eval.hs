module Evaluating.Eval
(
    module Control.Monad.Identity,
    module Control.Monad.State,

    Eval,

    runEval,
) where

import Control.Monad.Identity
import Control.Monad.State

--import Evaluating.Heap (Heap, empty)
import Evaluating.Environment (Environment, empty)

type Eval a = StateT Environment Identity a

runEval :: Eval a -> (a, Environment)
runEval eval = runIdentity (runStateT eval empty)

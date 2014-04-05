module Evaluating.Eval
(
    module Control.Monad.Identity,
    module Control.Monad.State,

    Eval,

    runEval,
) where

import Control.Monad.Identity
import Control.Monad.State

import Evaluating.Heap (Heap, empty)

type Eval a = StateT Heap Identity a

runEval :: Eval a -> (a, Heap)
runEval eval = runIdentity (runStateT eval empty)

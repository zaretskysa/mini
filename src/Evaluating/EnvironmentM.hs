module Evaluating.EnvironmentM
(
    enterLexEnv,
    leaveLexEnv,
    insertValue,
    lookupValue,
) where

import qualified Control.Monad.State as St

import Types
import Evaluating.Eval
import qualified Evaluating.Environment as E

enterLexEnv :: Eval ()
enterLexEnv = St.modify E.pushEmptyLexEnv

leaveLexEnv :: Eval ()
leaveLexEnv = St.modify E.removeTop

insertValue :: Identifier -> Value -> Eval ()
insertValue ident value = St.modify $ E.insertValue ident value

lookupValue :: Identifier -> Eval MaybeValue
lookupValue ident = St.gets $ E.lookupValue ident

module Evaluating.EnvironmentM
(
    enterLexEnv,
    leaveLexEnv,
    insertValue,
    insertValues,
    lookupValue,
) where

import qualified Control.Monad.State as St

import Types
import Evaluating.Value
import Evaluating.Eval
import qualified Evaluating.Environment as E

enterLexEnv :: Eval ()
enterLexEnv = St.modify E.pushEmptyLexEnv

leaveLexEnv :: Eval ()
leaveLexEnv = St.modify E.removeTop

insertValue :: Identifier -> Value -> Eval ()
insertValue ident value = St.modify $ E.insertValue ident value

insertValues :: [(Identifier, Value)] -> Eval ()
insertValues values = mapM_ inserter values
    where inserter (ident, value) = insertValue ident value

lookupValue :: Identifier -> Eval MaybeValue
lookupValue ident = St.gets $ E.lookupValue ident

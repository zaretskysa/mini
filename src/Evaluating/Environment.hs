module Evaluating.Environment
(
    Environment,

    empty,
    pushLexEnv,
    pushEmptyLexEnv,
    popLexEnv,
    removeTop,
    insertValue,
    lookupValue,
) where

import qualified Stack as S
import qualified Evaluating.Heap as H
import Evaluating.Value
import Types

type LexEnv = H.Heap

type LexicalStack = S.Stack LexEnv

data Environment = Environment LexicalStack
    deriving (Show)

empty :: Environment
empty = Environment $ S.empty

pushLexEnv :: LexEnv -> Environment -> Environment
pushLexEnv lexEnv (Environment stack) = Environment $ S.push lexEnv stack

pushEmptyLexEnv :: Environment -> Environment
pushEmptyLexEnv env = pushLexEnv H.empty env

popLexEnv :: Environment -> (LexEnv, Environment)
popLexEnv (Environment stack) =
    let (lexEnv, newStack) = S.pop stack
    in (lexEnv, Environment newStack)

removeTop :: Environment -> Environment
removeTop (Environment stack) = Environment $ S.removeTop stack

insertValue :: Identifier -> Value -> Environment -> Environment
insertValue ident value (Environment stack)
    | S.null stack = error "Environment has no any lexical scopes"
    | otherwise =
        let modifier = H.insert ident value
            newStack = S.modifyTop modifier stack
        in Environment newStack

lookupValue :: Identifier -> Environment -> MaybeValue
lookupValue ident (Environment stack)
    | S.null stack = error "Environment has no any lexical scopes"
    | otherwise = lookupValueInStack ident stack

lookupValueInStack :: Identifier -> LexicalStack -> MaybeValue
lookupValueInStack ident stack
    | S.null stack = Nothing
    | otherwise =
        let (env, reducedStack) = S.pop stack
        in case H.lookup ident env of
            Nothing -> lookupValueInStack ident reducedStack
            value -> value

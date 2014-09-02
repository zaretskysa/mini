module Evaluating.LexicalStack
(
    LexicalStack,

    new,

    pushEmptyScope,
    destroyScope,

    createRef,
    lookupRef,
    lookupValue,

) where

import Types
import Evaluating.Value
import qualified Stack
import qualified Evaluating.Scope as Scope
import Evaluating.RefHeap (RefHeap)

type LexicalStack = Stack.Stack Scope.Scope

new :: LexicalStack
new = Stack.new

pushEmptyScope :: LexicalStack -> LexicalStack
pushEmptyScope stack = 
    Stack.push Scope.new stack

destroyScope :: LexicalStack -> RefHeap -> (LexicalStack, RefHeap)
destroyScope stack heap =
    let (scope, newStack) = Stack.pop stack
        newHeap = Scope.destroy scope heap
    in (newStack, newHeap)

createRef :: Identifier -> Value -> LexicalStack -> RefHeap -> (Reference, LexicalStack, RefHeap)
createRef ident val stack heap =
    let (scope, topless) = Stack.pop stack
        (ref, newScope, newHeap) = Scope.createRef ident val scope heap
        newStack = Stack.push newScope topless
    in (ref, newStack, newHeap)

lookupRef :: Identifier -> LexicalStack -> Maybe Reference
lookupRef ident stack
    | Stack.null stack = Nothing
    | otherwise = 
        let (scope, rest) = Stack.pop stack
        in case Scope.lookupRef ident scope of
            Nothing -> lookupRef ident rest
            ref -> ref

lookupValue :: Identifier -> LexicalStack -> RefHeap -> MaybeValue
lookupValue ident stack heap
    | Stack.null stack = Nothing
    | otherwise = 
        let (scope, rest) = Stack.pop stack
        in case Scope.lookupValue ident scope heap of
            Nothing -> lookupValue ident rest heap
            value -> value

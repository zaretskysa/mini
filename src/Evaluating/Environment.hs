module Evaluating.Environment
(
    Environment,

    empty,
    pushEmptyLexEnv,
    removeTop,

    insertValue,
    lookupValue,

) where

import qualified Evaluating.LexicalStack as Stack
import Evaluating.Value
import Types

import qualified Evaluating.RefHeap as Heap

data Environment = Environment 
    { _stack :: Stack.LexicalStack
    , _heap :: Heap.RefHeap
    } deriving (Show)

empty :: Environment
empty = Environment {_stack = Stack.new, _heap = Heap.new }

pushEmptyLexEnv :: Environment -> Environment
pushEmptyLexEnv env =
    let newStack = Stack.pushEmptyScope $ _stack env
    in env {_stack = newStack}

removeTop :: Environment -> Environment
removeTop env@Environment{_stack = stack, _heap = heap} =
    let (newStack, newHeap) = Stack.destroyScope stack heap
    in env {_stack = newStack, _heap = newHeap}

insertValue :: Identifier -> Value -> Environment -> Environment
insertValue ident value env@Environment{_stack = stack, _heap = heap} =
    let (_ref, newStack, newHeap) = Stack.createRef ident value stack heap
    in env {_stack = newStack, _heap = newHeap}    

lookupValue :: Identifier -> Environment -> MaybeValue
lookupValue ident Environment{_stack = stack, _heap = heap} =
    Stack.lookupValue ident stack heap

module Evaluating.Scope
(
    Scope,
    new,
    destroy,

    createRef,
    lookupRef,
    lookupValue,
    deleteRef,
) where

import Prelude hiding (lookup)
import Data.Map (Map)
import qualified Data.Map as M

import Types
import Evaluating.Value

import Evaluating.RefHeap (RefHeap)
import qualified Evaluating.RefHeap as H

type Scope = Map Identifier Reference

new :: Scope
new = M.empty

destroy :: Scope -> RefHeap -> RefHeap
destroy scope heap =
    let refs = M.elems scope
    in H.deleteRefs refs heap

createRef :: Identifier -> Value -> Scope -> RefHeap -> (Reference, Scope, RefHeap)
createRef ident val scope heap =
    let (ref, newHeap) = H.createRef val heap
        newScope = M.insert ident ref scope
    in (ref, newScope, newHeap)

lookupRef :: Identifier -> Scope -> Maybe Reference
lookupRef ident scope =
    M.lookup ident scope

lookupValue :: Identifier -> Scope -> RefHeap -> Maybe Value
lookupValue ident scope heap =
    case lookupRef ident scope of
        Nothing -> Nothing
        Just ref -> H.lookupRef ref heap

deleteRef :: Identifier -> Scope -> RefHeap -> (Scope, RefHeap)
deleteRef ident scope heap =
    case lookupRef ident scope of
        Nothing -> error $ "Can not delete ref: scope does not contain ident " ++ ident
        Just ref -> 
            let newHeap = H.deleteRef ref heap
                newScope = M.delete ident scope
            in (newScope, newHeap)

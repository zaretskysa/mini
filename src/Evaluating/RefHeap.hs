module Evaluating.RefHeap
(
    RefHeap,

    new,
    createRef,
    copyRef,
    deleteRef,
    deleteRefs,
    lookupRef,
) where

import Data.Map (Map)
import qualified Data.Map as M

import Types
import Evaluating.Value
import Evaluating.Heap (Heap)
import qualified Evaluating.Heap as H

data RefHeap = RefHeap
    { _storage :: Heap Value
    , _refs :: Map Reference HeapObjId
    , _nextRefId :: Reference
    } deriving (Show)

new :: RefHeap
new = RefHeap 
    { _storage = H.new
    , _refs = M.empty
    , _nextRefId = 0
    }

createRef :: Value -> RefHeap -> (Reference, RefHeap)
createRef val heap =
    let (objId, newStorage) = H.insert val $ _storage heap
        refId = _nextRefId heap
        newRefs = M.insert refId objId (_refs heap)
        newHeap = heap {_storage = newStorage, _refs = newRefs, _nextRefId = refId + 1}
    in (refId, newHeap)

copyRef :: Reference -> RefHeap -> (Reference, RefHeap)
copyRef ref heap =
    case M.lookup ref $ _refs heap of
        Nothing -> error $ "Can not copy ref, no such ref: " ++ show ref
        Just objId ->
            let newStorage = H.accuire objId $ _storage heap
                refId = _nextRefId heap
                newRefs = M.insert refId objId $ _refs heap
                newHeap = heap {_storage = newStorage, _refs = newRefs, _nextRefId = refId + 1}
            in (refId, newHeap)

deleteRef :: Reference -> RefHeap -> RefHeap
deleteRef ref heap =
    case M.lookup ref $ _refs heap of
        Nothing -> error $ "Can not delete ref, no such ref: " ++ show ref
        Just objId ->
            let newStorage = H.release objId $ _storage heap
                newRefs = M.delete ref $ _refs heap
            in heap {_storage = newStorage, _refs = newRefs}

deleteRefs :: [Reference] -> RefHeap -> RefHeap
deleteRefs [] heap = heap
deleteRefs (ref:refs) heap =
    let newHeap = deleteRef ref heap
    in deleteRefs refs newHeap

lookupRef :: Reference -> RefHeap -> MaybeValue
lookupRef ref heap =
    case M.lookup ref $ _refs heap of
        Nothing -> Nothing
        Just objId ->
            case H.lookup objId $ _storage heap of
                Nothing -> error $ "ref " ++ show ref ++ "existts, but it refers to not existing objId " ++ show objId
                val@(Just _) -> val


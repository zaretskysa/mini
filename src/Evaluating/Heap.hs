module Evaluating.Heap
(
    Heap,

    new,
    insert,
    lookup,
    delete,
    accuire,
    release,    
) where

import Prelude hiding (lookup)
import Data.Map (Map)
import qualified Data.Map as M

import Types

data Heap a = Heap 
    { _storage :: Map HeapObjId (Element a)
    , _nextId :: HeapObjId
    } deriving (Show, Eq)

data Element a = Element
    { _value :: a
    , _refCount :: Integer
    } deriving (Show, Eq)

new :: Heap a
new = Heap {_storage = M.empty, _nextId = 0}

insert :: a -> Heap a -> (HeapObjId, Heap a)
insert val heap@(Heap {_storage = storage, _nextId = nextId}) = 
    let newCounter = 1 + nextId 
        newElement = Element {_value = val, _refCount = 1}
        newStorage = M.insert newCounter newElement storage
        newHeap = heap {_storage = newStorage, _nextId = newCounter}
    in (newCounter, newHeap)

lookup :: HeapObjId -> Heap a -> Maybe a
lookup objId (Heap {_storage = storage}) = 
    case M.lookup objId storage of
        Nothing -> Nothing
        Just (Element {_value = val}) -> Just val

delete :: HeapObjId -> Heap a -> Heap a
delete objId heap =
    let newStorage = M.delete objId $ _storage heap
    in heap {_storage = newStorage}

accuire :: HeapObjId -> Heap a -> Heap a
accuire objId heap =
    case lookupElement objId heap of
        Nothing -> error $ "Can not accuaire: objId does not exist: " ++ show (objId)
        Just element@(Element {_refCount = refCount}) -> 
            let newElement = element {_refCount = refCount + 1}
            in insertElement objId newElement heap

release :: HeapObjId -> Heap a -> Heap a
release objId heap = 
    case lookupElement objId heap of
        Nothing -> error $ "Can not release: objId does not exist: " ++ show (objId)
        Just (Element {_refCount = 1}) -> delete objId heap
        Just element@(Element {_refCount = refCount}) -> 
            let newElement = element {_refCount = refCount - 1}
            in insertElement objId newElement heap

lookupElement :: HeapObjId -> Heap a -> Maybe (Element a)
lookupElement objId (Heap {_storage = storage}) = 
    M.lookup objId storage

insertElement :: HeapObjId -> Element a -> Heap a -> Heap a
insertElement objId element heap@(Heap {_storage = storage}) =
    let newStorage = M.insert objId element storage
    in heap {_storage = newStorage}

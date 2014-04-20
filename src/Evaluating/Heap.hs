module Evaluating.Heap
(
    Heap,
    empty,
    insert,
    lookup,
    delete,
) where

import Prelude hiding (lookup)
import Data.Map (Map)
import qualified Data.Map as M

import Types
import Evaluating.Value

type Heap = Map Identifier Value

empty :: Heap
empty = M.empty

insert :: Identifier -> Value -> Heap -> Heap
insert = M.insert

lookup :: Identifier -> Heap -> Maybe Value
lookup = M.lookup

delete :: Identifier -> Heap -> Heap
delete = M.delete

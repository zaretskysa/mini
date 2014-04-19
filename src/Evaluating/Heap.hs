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

type Heap = Map Identifier Double

empty :: Heap
empty = M.empty

insert :: Identifier -> Double -> Heap -> Heap
insert = M.insert

lookup :: Identifier -> Heap -> Maybe Double
lookup = M.lookup

delete :: Identifier -> Heap -> Heap
delete = M.delete

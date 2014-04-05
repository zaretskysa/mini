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

type Heap = Map String Double

empty :: Heap
empty = M.empty

insert :: String -> Double -> Heap -> Heap
insert = M.insert

lookup :: String -> Heap -> Maybe Double
lookup = M.lookup

delete :: String -> Heap -> Heap
delete = M.delete

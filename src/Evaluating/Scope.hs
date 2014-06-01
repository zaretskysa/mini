module Evaluating.Scope
(
    Scope,
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

type Scope = Map Identifier Value

empty :: Scope
empty = M.empty

insert :: Identifier -> Value -> Scope -> Scope
insert = M.insert

lookup :: Identifier -> Scope -> Maybe Value
lookup = M.lookup

delete :: Identifier -> Scope -> Scope
delete = M.delete

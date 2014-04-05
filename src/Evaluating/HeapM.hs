module Evaluating.HeapM
(
    insert,
    lookup,
    delete,
) where

import Prelude hiding (lookup)
import qualified Control.Monad.State as St

import Evaluating.Eval
import qualified Evaluating.Heap as H

insert :: String -> Double -> Eval ()
insert varName value = St.modify $ H.insert varName value

lookup :: String -> Eval (Maybe Double)
lookup varName = St.gets $ H.lookup varName

delete :: String -> Eval ()
delete varName = St.modify $ H.delete varName

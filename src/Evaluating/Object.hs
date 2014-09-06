module Evaluating.Object
(
    Object,

    new,
    setProperty,
    getProperty,
    deleteProperty,
) where

import qualified Data.Map as Map

import Types
import Evaluating.Value (Value, MaybeValue)

data Object = Object
    { _props :: Map.Map String Value
    } deriving (Show, Eq)

new :: Object
new = Object {_props = Map.empty}

setProperty :: Identifier -> Value -> Object -> Object
setProperty name val obj =
    let newProps = Map.insert name val $ _props obj
    in obj {_props = newProps}

getProperty :: Identifier -> Object -> MaybeValue
getProperty name obj =
    Map.lookup name $ _props obj

deleteProperty :: Identifier -> Object -> Object
deleteProperty name obj =
    let newProps = Map.delete name $ _props obj
    in obj {_props = newProps}

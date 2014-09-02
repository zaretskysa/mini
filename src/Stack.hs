module Stack
(
    Stack,

    new,
    push,
    pop,
    removeTop,
    null,
    top,
    modifyTop,
) where

import Prelude hiding (lookup, null)
import qualified Data.List as List

data Stack a = Stack [a] deriving (Show)

new :: Stack a
new = Stack []

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

pop :: Stack a -> (a, Stack a)
pop (Stack (x:xs)) = (x, Stack xs)
pop (Stack []) = error "Trying to perform pop operation on empty stack"

removeTop :: Stack a -> Stack a
removeTop stack = snd $ pop stack

null :: Stack a -> Bool
null (Stack xs) = List.null xs

top :: Stack a -> a
top (Stack (x:_xs)) = x
top (Stack []) = error "Trying to perform pop operation on empty stack"

modifyTop :: (a -> a) -> Stack a -> Stack a
modifyTop fun stack =
    let (value, reduced) = pop stack
    in push (fun value) reduced

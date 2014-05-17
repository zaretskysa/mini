import Control.Monad.Identity
import Control.Monad.Cont
import Control.Monad.State

type MyState = String

data Statement =
      Return Int
    | GetState
    | SetState MyState
    | FuncCall [Statement]
    deriving (Show)

data Value = 
      Undefined
    | Value Int 
    | StateValue MyState
    deriving (Show)

type Eval a = ContT Value (State MyState) a

runEval ::(Eval Value) -> MyState -> (Value, MyState)
runEval eval state = runState (runContT eval return) state 

evalProg :: [Statement] -> Value
evalProg stmts = fst $ runEval (evalBlock stmts) $ ""

evalBlock :: [Statement] -> Eval Value
evalBlock [] = return Undefined
evalBlock [stmt] = evalStatment stmt
evalBlock (st:stmts) = evalStatment st >> evalBlock stmts

evalStatment :: Statement -> Eval Value
evalStatment (Return val) = funcReturn $ Value val
evalStatment (SetState state) = put state >> return Undefined
evalStatment (FuncCall stmts) = funcCall stmts
evalStatment GetState = get >>= return . StateValue

funcCall :: [Statement] -> Eval Value
funcCall stmts = lift $ runContT (evalBlock stmts) return

funcReturn :: Value -> Eval Value
funcReturn val = ContT $ \_ -> return val


test0 = evalProg [FuncCall [Return 1]] -- result is Value 1
test1 = evalProg [FuncCall [Return 1], Return 2] -- result is Value 2
test2 = evalProg [SetState "Hello", FuncCall [SetState "Galaxy", Return 3], GetState] -- result is StateValue "Galaxy"

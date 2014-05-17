import Control.Monad.Identity
import Control.Monad.Cont
import Control.Monad.State

data Statement =
      FuncCall [Statement]
    | Return Int
    deriving (Show)

data Value = 
      Undefined 
    | Value Int 
    deriving (Show)

type Eval a = Cont Value a

runEval :: Eval Value -> Value
runEval eval = runCont eval id

evalProg :: [Statement] -> Value
evalProg stmts = runEval $ evalBlock stmts

evalBlock :: [Statement] -> Eval Value
evalBlock [] = return Undefined
evalBlock [stmt] = evalStatment stmt
evalBlock (st:stmts) = evalStatment st >> evalBlock stmts

evalStatment :: Statement -> Eval Value
evalStatment (Return val) = cont $ \_ -> Value val
evalStatment (FuncCall stmts) = return $ runEval $ evalBlock stmts

test1 = evalProg [FuncCall [Return 1], Return 2] -- result is Value 2
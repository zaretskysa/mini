import Control.Monad.Identity
import Control.Monad.Cont
import Control.Monad.Error

data Statement = 
      Expr Int
    | Block [Statement]
    | Return Int
    | Throw Int
    | TryCatch Statement Statement
    deriving (Show, Eq)

data Value = 
      Undefined
    | Value Int
    deriving (Show, Eq)

data Exception = Exception Int deriving(Show, Eq)

instance Error Exception where
    noMsg = Exception 0
    strMsg str = Exception 0

type Eval a = ErrorT Exception (ContT (Either Exception Value) Identity) a

runEval :: Eval Value -> Either Exception Value
runEval eval = runIdentity (runContT (runErrorT eval) return)

evalStmt :: Statement -> Eval Value
evalStmt (Expr val) = return $ Value val
evalStmt (Block stmts) = evalBlock stmts
evalStmt (Return val) = returnValue $ Value val
evalStmt (Throw val) = throwError $ Exception val
evalStmt (TryCatch tryStmt catchStmt) = 
    evalStmt tryStmt `catchError` (\_e -> evalStmt catchStmt)


returnValue :: Value -> Eval Value
returnValue val = ErrorT (cont $ \_ -> Right val)

evalBlock :: [Statement] -> Eval Value
evalBlock [] = return Undefined
evalBlock [st] = evalStmt st
evalBlock (st:rest) = evalStmt st >> evalBlock rest

evalProgram :: [Statement] -> Either Exception Value
evalProgram stmts = runEval $ evalBlock stmts

prog1 = [Expr 1, Block [Return 3, Expr 2], Expr 4] 
test1 = evalProgram prog1 -- result will be Value 3

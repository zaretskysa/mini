module Error where

import Control.Monad.Identity
import Control.Monad.Cont
import Control.Monad.Error
import System.IO


data Statement = 
      Expr Int
    | Throw Int
    | Block [Statement]
    | TryCatch Statement Statement
    deriving (Show, Eq)

data Value =
      Undefined
    | Value Int
    deriving (Show, Eq)

data Exception = Exception Int deriving (Show, Eq)

instance Error Exception where
    noMsg = Exception (-1)
    strMsg str = Exception (-1)

type Eval a = ErrorT Exception Identity a

runEval eval = runIdentity (runErrorT eval)

eval :: [Statement] -> Either Exception Value
eval prog = runEval $ evalBlock prog

evalBlock :: [Statement] -> Eval Value
evalBlock [] = return Undefined
evalBlock [stmt] = evalStatement stmt
evalBlock (st:stmts) = evalStatement st >> evalBlock stmts

evalStatement :: Statement -> Eval Value
evalStatement (Expr val) = return $ Value val
evalStatement (Block stmts) = evalBlock stmts
evalStatement (Throw val) = throwError $ Exception val
evalStatement (TryCatch tryStmt catchStmt) = 
    evalStatement tryStmt `catchError` (\_e -> evalStatement catchStmt)


evalProg1 = eval [TryCatch (Throw 666) (Expr 111)]
evalProg2 = eval [TryCatch (Block [Throw 222 ]) (Expr 111)]

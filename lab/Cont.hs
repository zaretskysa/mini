module Cont where

import Control.Monad.Cont

data Statement = 
      Expr Int
    | Block [Statement]
    | Return Int
    deriving (Show, Eq)

data Value = 
      Undefined
    | Value Int
    deriving (Show, Eq)

evalStmt :: Statement -> Cont Value Value
evalStmt (Expr val) = return $ Value val
evalStmt (Block stmts) = evalBlock stmts
evalStmt (Return val) = returnValue $ Value val

returnValue :: Value -> Cont Value Value
returnValue val = cont $ \_ -> val

evalBlock :: [Statement] -> Cont Value Value
evalBlock [] = return Undefined
evalBlock [st] = evalStmt st
evalBlock (st:rest) = evalStmtAndContinue st rest

evalStmtAndContinue :: Statement -> [Statement] -> Cont Value Value
evalStmtAndContinue st rest =
    cont $ \next -> runCont (evalStmt st) (\_ -> runCont (evalBlock rest) next)

evalProgram :: [Statement] -> Value
evalProgram stmts = runCont (evalBlock stmts) id

prog1 = [Expr 1, Block [Return 3, Expr 2], Expr 4] 
evalProg1 = evalProgram prog1 -- result will be Value 3

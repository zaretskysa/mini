module Cont where

data Statement = 
      Expr Int
    | Block [Statement]
    | Return Int
    deriving (Show, Eq)

data Value = 
      Undefined
    | Value Int
    deriving (Show, Eq)

type NextStep = Value -> Value

evalStmt :: Statement -> NextStep -> Value
evalStmt (Expr val) next = 
    let res = Value val
    in next res
evalStmt (Block stmts) next = evalBlock stmts next
evalStmt (Return val) next = Value val

evalBlock :: [Statement] -> NextStep -> Value
evalBlock [] next = next Undefined
evalBlock [st] next = evalStmt st next
evalBlock (st:rest) next = evalStmt st $ \ _ -> evalBlock rest next

evalProgram stmts = evalBlock stmts id

prog1 = [Expr 1, Block [Return 3, Expr 2], Expr 4] 
evalProg1 = evalProgram prog1 -- result will be Value 3

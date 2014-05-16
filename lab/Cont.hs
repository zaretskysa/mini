module Cont where

import Control.Monad.Identity
import Control.Monad.Cont
import Control.Monad.State

data Statement = 
      Expr Int
    | Block [Statement]
    | Return Int
    deriving (Show, Eq)

data Value = 
      Undefined
    | Value Int
    deriving (Show, Eq)

type Eval a = StateT Int (ContT Value Identity) a

runEval :: Eval Value -> Value
runEval eval = runIdentity (runContT (runStateT eval 0) (return . fst))

evalStmt :: Statement -> Eval Value
evalStmt (Expr val) = (put val) >> (return $ Value val)
evalStmt (Block stmts) = evalBlock stmts
evalStmt (Return val) = (put val) >> (returnValue $ Value val)

returnValue :: Value -> Eval Value
returnValue val = lift $ cont $ \_ -> val

evalBlock :: [Statement] -> Eval Value
evalBlock [] = return Undefined
evalBlock [st] = evalStmt st
evalBlock (st:rest) = evalStmt st >> evalBlock rest

evalProgram :: [Statement] -> Value
evalProgram stmts = runEval $ evalBlock stmts

prog1 = [Expr 1, Block [Return 3, Expr 2], Expr 4] 
evalProg1 = evalProgram prog1 -- result will be Value 3

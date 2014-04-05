module Evaluating.Evaluator
(
    evalProgram,
    evalString
) where

import Debug.Trace
import Prelude hiding (lookup)

import Parsing.Ast
import Parsing.Parser
import Evaluating.Eval
import qualified Evaluating.HeapM as H

type BinaryOperator = Double -> Double -> Double

evalString :: String -> Double
evalString input = case parseString input of
    Left _ -> -1
    Right prog -> eval prog

eval :: Program -> Double
eval program = 
    let (result, heap) = runEval $ evalProgram program
    in traceShow heap result

evalProgram :: Program -> Eval Double
evalProgram (Program [stmt]) = evalStatement stmt
evalProgram _ = return $ -1

evalStatement :: Statement -> Eval Double
evalStatement (ExpressionStatement expr) = evalExpression expr
evalStatement (VarDeclStatement varName expr) = do
    value <- evalExpression expr
    H.insert varName value
    return value

evalExpression :: Expression -> Eval Double
evalExpression (UnaryExpression mult) = evalMultExpression mult
evalExpression (PlusExpression expr mult) = evalBinaryExpr expr mult (+)
evalExpression (MinusExpression expr mult) = evalBinaryExpr expr mult (-)

evalBinaryExpr :: Expression -> MultExpression -> BinaryOperator -> Eval Double
evalBinaryExpr expr mult op = do 
    left <- evalExpression expr
    right <- evalMultExpression mult
    return $ left `op` right

evalMultExpression :: MultExpression -> Eval Double
evalMultExpression (UnaryMultExpression num) = return num
evalMultExpression (MultMultExpression mult num) = evalBinaryMultExpr mult num (*)
evalMultExpression (DivMultExpression mult num) = evalBinaryMultExpr mult num (/)

evalBinaryMultExpr :: MultExpression -> Double -> BinaryOperator -> Eval Double
evalBinaryMultExpr mult num op = do 
    left <- evalMultExpression mult
    return $ left `op` num

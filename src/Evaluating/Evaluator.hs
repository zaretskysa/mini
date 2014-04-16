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

evalString :: String -> Maybe Double
evalString input = case parseString input of
    Left _ -> Nothing
    Right prog -> eval prog

eval :: Program -> Maybe Double
eval program = 
    let (result, heap) = runEval $ evalProgram program
    in traceShow heap result

evalProgram :: Program -> Eval (Maybe Double)
evalProgram (Program []) = return Nothing
evalProgram (Program stmts) = do
    results <- mapM evalStatement stmts
    return $ last results

evalStatement :: Statement -> Eval (Maybe Double)
evalStatement (ExpressionStatement expr) = do
    value <- evalAssignmentExpression expr
    return $ Just value
evalStatement (VarDeclStatement varName expr) = do
    value <- evalAddiitiveExpression expr
    H.insert varName value
    return $ Just value

evalAssignmentExpression :: AssignmentExpression -> Eval Double
evalAssignmentExpression (AdditiveAssignmentExpression expr) = evalAddiitiveExpression expr
evalAssignmentExpression (AssignmentOperatorExpression varName expr) = do
    value <- evalAddiitiveExpression expr
    H.insert varName value
    return value

evalAddiitiveExpression :: AdditiveExpression -> Eval Double
evalAddiitiveExpression (UnaryAdditiveExpression mult) = evalMultExpression mult
evalAddiitiveExpression (PlusExpression expr mult) = evalBinaryExpr expr mult (+)
evalAddiitiveExpression (MinusExpression expr mult) = evalBinaryExpr expr mult (-)

evalBinaryExpr :: AdditiveExpression -> MultExpression -> BinaryOperator -> Eval Double
evalBinaryExpr expr mult op = do 
    left <- evalAddiitiveExpression expr
    right <- evalMultExpression mult
    return $ left `op` right

evalMultExpression :: MultExpression -> Eval Double
evalMultExpression (UnaryMultExpression acc) = evalAccessExpression acc
evalMultExpression (MultMultExpression mult acc) = evalBinaryMultExpr mult acc (*)
evalMultExpression (DivMultExpression mult acc) = evalBinaryMultExpr mult acc (/)

evalBinaryMultExpr :: MultExpression -> AccessExpression -> BinaryOperator -> Eval Double
evalBinaryMultExpr mult acc op = do
    left <- evalMultExpression mult
    right <- evalAccessExpression acc
    return $ left `op` right

evalAccessExpression :: AccessExpression -> Eval Double
evalAccessExpression (DoubleAccessExpression num) = return num
evalAccessExpression (IdentAccessExpression ident) = do
    Just val <- H.lookup ident
    return val


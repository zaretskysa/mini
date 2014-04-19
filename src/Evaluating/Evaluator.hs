module Evaluating.Evaluator
(
    evalProgram,
    evalString
) where

import Debug.Trace
import Prelude hiding (lookup)

import Debug
import Types
import Parsing.Ast
import Parsing.Parser
import Evaluating.Eval
import qualified Evaluating.EnvironmentM as E

type BinaryOperator = Value -> Value -> Value

evalString :: String -> MaybeValue
evalString input = case parseString input of
    Left _ -> Nothing
    Right prog -> eval prog

eval :: Program -> MaybeValue
eval program = 
    let (result, env) = runEval $ evalProgram program
    in traceShow env result

evalProgram :: Program -> Eval MaybeValue
evalProgram (Program []) = return Nothing
evalProgram (Program elements) = do
    E.enterLexEnv
    results <- mapM evalSourceElement elements
    return $ last results

evalSourceElement :: SourceElement -> Eval MaybeValue
evalSourceElement (StatementSourceElement stmt) = evalStatement stmt
evalSourceElement (FunctionDeclSourceElement _name _params _body) = return Nothing

evalStatement :: Statement -> Eval MaybeValue
evalStatement (ExpressionStatement expr) = do
    value <- evalAssignmentExpression expr
    return $ Just value
evalStatement (VarDeclStatement ident expr) = do
    value <- evalAddiitiveExpression expr
    E.insertValue ident value
    return $ Just value

evalAssignmentExpression :: AssignmentExpression -> Eval Value
evalAssignmentExpression (AdditiveAssignmentExpression expr) = evalAddiitiveExpression expr
evalAssignmentExpression (AssignmentOperatorExpression varName expr) = do
    value <- evalAddiitiveExpression expr
    E.insertValue varName value
    return value

evalAddiitiveExpression :: AdditiveExpression -> Eval Value
evalAddiitiveExpression (UnaryAdditiveExpression mult) = evalMultExpression mult
evalAddiitiveExpression (PlusExpression expr mult) = evalBinaryExpr expr mult (+)
evalAddiitiveExpression (MinusExpression expr mult) = evalBinaryExpr expr mult (-)

evalBinaryExpr :: AdditiveExpression -> MultExpression -> BinaryOperator -> Eval Value
evalBinaryExpr expr mult op = do 
    left <- evalAddiitiveExpression expr
    right <- evalMultExpression mult
    return $ left `op` right

evalMultExpression :: MultExpression -> Eval Value
evalMultExpression (UnaryMultExpression acc) = evalAccessExpression acc
evalMultExpression (MultMultExpression mult acc) = evalBinaryMultExpr mult acc (*)
evalMultExpression (DivMultExpression mult acc) = evalBinaryMultExpr mult acc (/)

evalBinaryMultExpr :: MultExpression -> AccessExpression -> BinaryOperator -> Eval Value
evalBinaryMultExpr mult acc op = do
    left <- evalMultExpression mult
    right <- evalAccessExpression acc
    return $ left `op` right

evalAccessExpression :: AccessExpression -> Eval Value
evalAccessExpression (DoubleAccessExpression num) = return num
evalAccessExpression (IdentAccessExpression ident) = do
    Just val <- E.lookupValue ident
    return val
evalAccessExpression (CallAccessExpression func params) = $stub

module Evaluating.Evaluator
(
    evalProgram,
    evalString
) where

import Debug.Trace
import Prelude hiding (lookup)
import Text.Show.Pretty (ppShow)

import Debug
import Parsing.Ast
import Parsing.Parser
import Evaluating.Eval
import Evaluating.Value
import qualified Evaluating.EnvironmentM as E

type BinaryOperator = Double -> Double -> Double

evalString :: String -> Value
evalString input = case parseString input of
    Left _ -> UndefinedValue
    Right prog -> eval prog

eval :: Program -> Value
eval program = 
    let (result, env) = runEval $ evalProgram program
    in trace (ppShow env) result

evalProgram :: Program -> Eval Value
evalProgram (Program []) = return UndefinedValue
evalProgram (Program elements) = E.enterLexEnv >> evalSourceElements elements

evalSourceElements :: [SourceElement] -> Eval Value
evalSourceElements [] = return UndefinedValue
evalSourceElements [element] = evalSourceElement element
evalSourceElements (element:rest) = evalSourceElement element >> evalSourceElements rest

evalStatements :: [Statement] -> Eval Value
evalStatements [] = return UndefinedValue
evalStatements [stmt] = evalStatement stmt
evalStatements (st:stmts) = evalStatement st >> evalStatements stmts

evalSourceElement :: SourceElement -> Eval Value
evalSourceElement (StatementSourceElement stmt) = evalStatement stmt
evalSourceElement (FunctionDeclSourceElement decl) =
    evalFunctionDecl decl >> return UndefinedValue

evalFunctionDecl :: FunctionDeclaration -> Eval ()
evalFunctionDecl func@(FunctionDeclaration name _params _body) =
    E.insertValue name $ FunctionValue func

evalStatement :: Statement -> Eval Value
evalStatement EmptyStatement = return UndefinedValue
evalStatement (BlockStatement stmts) = do
    E.enterLexEnv
    res <- evalStatements stmts
    E.leaveLexEnv
    return res
evalStatement (ExpressionStatement expr) = do
    value <- evalAssignmentExpression expr
    return value
evalStatement (VarDeclStatement ident expr) = do
    value <- evalAdditiveExpression expr
    E.insertValue ident value
    return value
evalStatement (IfStatement expr thenStmt mbElseStmt) =
    evalAdditiveExpression expr >>= evalIfThenElse
    where
        evalIfThenElse value
            | NumberValue num <- value, num /= 0 = evalStatement thenStmt
            | Just elseStmt <- mbElseStmt = evalStatement elseStmt
            | otherwise = return UndefinedValue
evalStatement (ReturnStatement mbExpr) = return UndefinedValue

evalAssignmentExpression :: AssignmentExpression -> Eval Value
evalAssignmentExpression (AdditiveAssignmentExpression expr) = evalAdditiveExpression expr
evalAssignmentExpression (AssignmentOperatorExpression varName expr) = do
    value <- evalAdditiveExpression expr
    E.insertValue varName value
    return value

evalAdditiveExpression :: AdditiveExpression -> Eval Value
evalAdditiveExpression (UnaryAdditiveExpression mult) = evalMultExpression mult
evalAdditiveExpression (PlusExpression expr mult) = evalBinaryExpr expr mult (+)
evalAdditiveExpression (MinusExpression expr mult) = evalBinaryExpr expr mult (-)

evalBinaryExpr :: AdditiveExpression -> MultExpression -> BinaryOperator -> Eval Value
evalBinaryExpr expr mult op = do 
    NumberValue left <- evalAdditiveExpression expr
    NumberValue right <- evalMultExpression mult
    return $ NumberValue $ left `op` right

evalMultExpression :: MultExpression -> Eval Value
evalMultExpression (UnaryMultExpression acc) = evalAccessExpression acc
evalMultExpression (MultMultExpression mult acc) = evalBinaryMultExpr mult acc (*)
evalMultExpression (DivMultExpression mult acc) = evalBinaryMultExpr mult acc (/)

evalBinaryMultExpr :: MultExpression -> AccessExpression -> BinaryOperator -> Eval Value
evalBinaryMultExpr mult acc op = do
    (NumberValue left) <- evalMultExpression mult
    (NumberValue right) <- evalAccessExpression acc
    return $ NumberValue $ left `op` right

evalAccessExpression :: AccessExpression -> Eval Value
evalAccessExpression (DoubleAccessExpression num) = return $ NumberValue num
evalAccessExpression (IdentAccessExpression ident) = do
    Just val <- E.lookupValue ident
    return val
evalAccessExpression (CallAccessExpression funcName actualParams) = do
    Just (FunctionValue (FunctionDeclaration _ formalParams body)) <- E.lookupValue funcName
    evaluatedParams <- mapM evalAssignmentExpression actualParams
    E.enterLexEnv
    zipWithM E.insertValue formalParams evaluatedParams
    result <- evalSourceElements body
    E.leaveLexEnv
    return result

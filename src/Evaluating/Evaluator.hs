module Evaluating.Evaluator
(
    evalProgram,
    evalString
) where

import Debug.Trace
import Prelude hiding (lookup)
import Text.Show.Pretty (ppShow)
import Control.Monad.Cont

import Types
import Parsing.Ast
import Parsing.Parser
import Evaluating.Eval
import Evaluating.Value
import Evaluating.ConversionM
import qualified Evaluating.EnvironmentM as E

type BinaryOperator = Double -> Double -> Double

evalString :: String -> Value
evalString input = case parseString input of
    Left _ -> UndefinedValue
    Right prog -> eval prog

eval :: Program -> Value
eval program = 
    let (val, env) = runEval $ evalProgram program
    in trace (ppShow env) val

evalProgram :: Program -> Eval Value
evalProgram (Program []) = return UndefinedValue
evalProgram (Program elements) = E.enterLexEnv >> evalSourceElements elements

evalSourceElements :: [SourceElement] -> Eval Value
evalSourceElements [] = return UndefinedValue
evalSourceElements [element] = evalSourceElement element >> return UndefinedValue
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
evalStatement (BlockStatement stmts) = evalStatements stmts
evalStatement (ExpressionStatement expr) = evalAssignmentExpression expr
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
evalStatement (ReturnStatement Nothing) = return UndefinedValue
evalStatement (ReturnStatement (Just expr)) = 
    evalLogicalAndExpression expr >>= evalFuncReturn

evalFuncReturn :: Value -> Eval Value
evalFuncReturn val = ContT $ \_ -> return val

evalAssignmentExpression :: AssignmentExpression -> Eval Value
evalAssignmentExpression (LogicalAndAssignmentExpression expr) = 
    evalLogicalAndExpression expr
evalAssignmentExpression (AssignmentOperatorExpression varName expr) = do
    value <- evalLogicalAndExpression expr
    E.insertValue varName value
    return value

evalLogicalAndExpression :: LogicalAndExpression -> Eval Value
evalLogicalAndExpression (UnaryLogicalAndExpression expr) = evalAdditiveExpression expr
evalLogicalAndExpression (BinaryLogicalAndExpression logical additive) = do
    left <- evalLogicalAndExpression logical >>= toBool
    right <- evalAdditiveExpression additive >>= toBool
    return $ BoolValue $ left && right

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
    evalFuncCall body $ zip formalParams evaluatedParams

evalFuncCall :: [SourceElement] -> [(Identifier, Value)] -> Eval Value
evalFuncCall body params = do
    E.enterLexEnv
    E.insertValues params
    val <- lift $ runContT (evalSourceElements body) return
    E.leaveLexEnv
    return val

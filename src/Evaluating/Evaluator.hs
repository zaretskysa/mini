module Evaluating.Evaluator
(
    evalProgram,
    evalString
) where

import Debug.Trace
import Prelude hiding (lookup)
import Text.Show.Pretty (ppShow)
import Control.Monad.Cont
import Control.Monad.Error

import Types
import Parsing.Ast
import Parsing.Parser
import Evaluating.Eval
import Evaluating.Value
import Evaluating.ConversionM
import qualified Evaluating.EnvironmentM as Env
import qualified Evaluating.ExceptionM as Ex

type BinaryOperator = Double -> Double -> Double


evalString :: String -> Either Value Value
evalString input = case parseString input of
    Left _ -> Left $ StringValue "parse_error"
    Right prog -> eval prog

eval :: Program -> Either Value Value
eval program = 
    let (res, env) = runEval $ evalProgram program
    in trace (ppShow env) res

evalProgram :: Program -> Eval Value
evalProgram (Program []) = return UndefinedValue
evalProgram (Program elements) = Env.enterLexEnv >> evalSourceElements elements

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
    Env.insertValue name $ FunctionValue func

evalStatement :: Statement -> Eval Value
evalStatement EmptyStatement = return UndefinedValue
evalStatement (BlockStatement stmts) = evalStatements stmts
evalStatement (ExpressionStatement expr) = evalAssignmentExpression expr
evalStatement (VarDeclStatement ident expr) = do
    value <- evalAdditiveExpression expr
    Env.insertValue ident value
    return value
evalStatement (IfStatement expr thenStmt mbElseStmt) =
    evalIfThenElse expr thenStmt mbElseStmt
evalStatement (ReturnStatement Nothing) = return UndefinedValue
evalStatement (ReturnStatement (Just expr)) = 
    evalLogicalOrExpression expr >>= evalFuncReturn
evalStatement (ThrowStatement expr) = do
    val <- evalLogicalOrExpression expr
    Ex.throw val
evalStatement (TryCatchStatement tryBlock (Catch ident catchBlock)) = do
    evalStatements tryBlock `catchError` handler
    where 
        handler ex = do
            Env.enterLexEnv
            Env.insertValue ident ex
            result <- evalStatements catchBlock
            Env.leaveLexEnv
            return result

evalIfThenElse :: LogicalOrExpression -> Statement -> MaybeStatement -> Eval Value
evalIfThenElse expr thenStmt mbElseStmt = do
    evalLogicalOrExpression expr >>= toBool >>= innerEvalIfThenElse
    where
        innerEvalIfThenElse bool
            | bool = evalStatement thenStmt
            | Just elseStmt <- mbElseStmt = evalStatement elseStmt
            | otherwise = return UndefinedValue

evalFuncReturn :: Value -> Eval Value
evalFuncReturn val = ErrorT $ ContT $ \_ -> return $ Right val

evalAssignmentExpression :: AssignmentExpression -> Eval Value
evalAssignmentExpression (LogicalOrAssignmentExpression expr) = 
    evalLogicalOrExpression expr
evalAssignmentExpression (AssignmentOperatorExpression varName expr) = do
    value <- evalLogicalOrExpression expr
    Env.insertValue varName value
    return value

evalLogicalOrExpression :: LogicalOrExpression -> Eval Value
evalLogicalOrExpression (UnaryLogicalOrExpression expr) = 
    evalLogicalAndExpression expr
evalLogicalOrExpression (BinaryLogicalOrExpression logical additive) = do
    left <- evalLogicalOrExpression logical >>= toBool
    right <- evalLogicalAndExpression additive >>= toBool
    return $ BoolValue $ left || right

evalLogicalAndExpression :: LogicalAndExpression -> Eval Value
evalLogicalAndExpression (UnaryLogicalAndExpression expr) = evalEqualityExpression expr
evalLogicalAndExpression (BinaryLogicalAndExpression logical equality) = do
    left <- evalLogicalAndExpression logical >>= toBool
    right <- evalEqualityExpression equality >>= toBool
    return $ BoolValue $ left && right

--TODO: remove copypaste
evalEqualityExpression :: EqualityExpression -> Eval Value
evalEqualityExpression (UnaryEqualityExpression expr) = evalAdditiveExpression expr
evalEqualityExpression (EqualsExpression equal add) = do
    left <- evalEqualityExpression equal
    right <- evalAdditiveExpression add
    return $ BoolValue $ left == right
evalEqualityExpression (NotEqualsExpression equal add) = do
    left <- evalEqualityExpression equal
    right <- evalAdditiveExpression add
    return $ BoolValue $ left /= right

evalAdditiveExpression :: AdditiveExpression -> Eval Value
evalAdditiveExpression (UnaryAdditiveExpression mult) = evalMultExpression mult
evalAdditiveExpression (PlusExpression expr mult) = evalBinaryExpr expr mult (+)
evalAdditiveExpression (MinusExpression expr mult) = evalBinaryExpr expr mult (-)

evalBinaryExpr :: AdditiveExpression -> MultExpression -> BinaryOperator -> Eval Value
evalBinaryExpr expr mult op = do 
    left <- evalAdditiveExpression expr >>= toDouble
    right <- evalMultExpression mult >>= toDouble
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
evalAccessExpression (BoolAccessExpression val) = return $ BoolValue val
evalAccessExpression (StringAccessExpression val) = return $ StringValue val
evalAccessExpression (IdentAccessExpression ident) = do
    res <- Env.lookupValue ident
    case res of
        Just val -> return val
        Nothing -> Ex.throwNoBinding ident
evalAccessExpression (CallAccessExpression funcName actualParams) = do
    res <- Env.lookupValue funcName
    case res of
        Nothing -> Ex.throwNoBinding funcName
        Just funcVal
            | FunctionValue (FunctionDeclaration _ formalParams body) <- funcVal -> do
                evaluatedParams <- mapM evalAssignmentExpression actualParams
                evalFuncCall body $ zip formalParams evaluatedParams
            | otherwise -> Ex.throwNotFunction funcName

evalFuncCall :: [SourceElement] -> [(Identifier, Value)] -> Eval Value
evalFuncCall body params = do
    Env.enterLexEnv
    Env.insertValues params
    evalResult <- lift $ lift $ runContT (runErrorT (evalSourceElements body)) return
    Env.leaveLexEnv
    case evalResult of
        Right value -> return value
        Left e -> Ex.throw e

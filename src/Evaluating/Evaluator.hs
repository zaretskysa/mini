module Evaluating.Evaluator
(
    evalProgram,
    evalString
) where

import Parsing.Ast
import Parsing.Parser

evalString :: String -> Integer
evalString input = case parseString input of
    Left _ -> -1
    Right prog -> evalProgram prog

evalProgram :: Program -> Integer
evalProgram (Program [stmt]) = evalStatement stmt
evalProgram _ = -1

evalStatement :: Statement -> Integer
evalStatement (ExpressionStatement expr) = evalExpression expr

evalExpression :: Expression -> Integer
evalExpression (UnaryExpression mult) = evalMultExpression mult
evalExpression (PlusExpression expr mult) = 
    let r1 = evalExpression expr
        r2 = evalMultExpression mult
    in r1 + r2
evalExpression (MinusExpression expr mult) = 
    let r1 = evalExpression expr
        r2 = evalMultExpression mult
    in r1 - r2

evalMultExpression :: MultExpression -> Integer
evalMultExpression (UnaryMultExpression num) = num
evalMultExpression (MultMultExpression mult num) = 
    let r1 = evalMultExpression mult
    in r1 * num
evalMultExpression (DivMultExpression mult num) = 
    let r1 = evalMultExpression mult
    in r1 

module Evaluating.Evaluator
(
    evalProgram,
    evalString
) where

import Parsing.Ast
import Parsing.Parser

evalString :: String -> Double
evalString input = case parseString input of
    Left _ -> -1
    Right prog -> evalProgram prog

evalProgram :: Program -> Double
evalProgram (Program [stmt]) = evalStatement stmt
evalProgram _ = -1

evalStatement :: Statement -> Double
evalStatement (ExpressionStatement expr) = evalExpression expr

evalExpression :: Expression -> Double
evalExpression (UnaryExpression mult) = evalMultExpression mult
evalExpression (PlusExpression expr mult) = 
    let r1 = evalExpression expr
        r2 = evalMultExpression mult
    in r1 + r2
evalExpression (MinusExpression expr mult) = 
    let r1 = evalExpression expr
        r2 = evalMultExpression mult
    in r1 - r2

evalMultExpression :: MultExpression -> Double
evalMultExpression (UnaryMultExpression num) = num
evalMultExpression (MultMultExpression mult num) = 
    let r1 = evalMultExpression mult
    in r1 * num
evalMultExpression (DivMultExpression mult num) = 
    let r1 = evalMultExpression mult
    in r1 / num

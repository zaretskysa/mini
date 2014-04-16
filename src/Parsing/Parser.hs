module Parsing.Parser
(
    parseTokens,
    parseString
) where

import Text.ParserCombinators.Parsec (ParseError)
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Prim hiding (token)

import Debug
import Lexing.Token
import Lexing.Lexer
import Parsing.Ast
import Parsing.TokenParser

parseTokens :: [Token] -> Either ParseError Program
parseTokens input = runParser program () "tokens parser" input

parseString :: String -> Either ParseError Program
parseString input = case tokenize input of
    Left err -> Left err
    Right toks -> parseTokens toks

program :: TokenParser Program
program = do
    stmts <- sepBy statement (punctuator SemicolonPunctuator)
    --punctuator SemicolonPunctuator
    eof
    return $ Program stmts

statement :: TokenParser Statement
statement = expressionStatement <|> varDeclStatement

expressionStatement :: TokenParser Statement
expressionStatement = assignmentExpression >>= return . ExpressionStatement

assignmentExpression :: TokenParser AssignmentExpression
assignmentExpression = 
    try assignmentOperatorExpression 
    <|> additiveAssignmentExpression

additiveAssignmentExpression :: TokenParser AssignmentExpression
additiveAssignmentExpression = additiveExpression >>= return . AdditiveAssignmentExpression

assignmentOperatorExpression :: TokenParser AssignmentExpression
assignmentOperatorExpression = do
    varName <- identifier
    punctuator AssignPunctuator
    expr <- additiveExpression
    return $ AssignmentOperatorExpression varName expr


varDeclStatement :: TokenParser Statement
varDeclStatement = do
    keyword VarKeyword
    varName <- identifier
    punctuator AssignPunctuator
    expr <- additiveExpression
    return $ VarDeclStatement varName expr

additiveExpression :: TokenParser AdditiveExpression
additiveExpression = do
    left <- unaryAdditiveExpression
    restOfExpression left

--TODO: try to use 'chainl' combinator to deal with left recursion
restOfExpression :: AdditiveExpression -> TokenParser AdditiveExpression
restOfExpression left = do
    (try $ restOfPlusExpression left)
    <|> (try $ restOfMinusExpression left)
    <|> return left

restOfPlusExpression :: AdditiveExpression -> TokenParser AdditiveExpression
restOfPlusExpression left = do
    punctuator PlusPunctuator
    mult <- multExpression
    restOfExpression $ PlusExpression left mult

restOfMinusExpression :: AdditiveExpression -> TokenParser AdditiveExpression
restOfMinusExpression left = do
    punctuator MinusPunctuator
    mult <- multExpression
    restOfExpression $ MinusExpression left mult

unaryAdditiveExpression :: TokenParser AdditiveExpression
unaryAdditiveExpression = multExpression >>= return . UnaryAdditiveExpression

multExpression :: TokenParser MultExpression
multExpression = do
    left <- unaryMultExpression
    restOfMultExpression left

unaryMultExpression :: TokenParser MultExpression
unaryMultExpression = accessExpression >>= return . UnaryMultExpression

restOfMultExpression :: MultExpression -> TokenParser MultExpression
restOfMultExpression left = do
    (try $ restOfMultMultExpression left)
    <|> (try $ restOfDivMultExpression left)
    <|> (return left)

restOfMultMultExpression :: MultExpression -> TokenParser MultExpression
restOfMultMultExpression left = do
    punctuator MultPunctuator
    acc <- accessExpression
    restOfMultExpression $ MultMultExpression left acc

restOfDivMultExpression :: MultExpression -> TokenParser MultExpression
restOfDivMultExpression left = do
    punctuator DivPunctuator
    acc <- accessExpression
    restOfMultExpression $ DivMultExpression left acc

accessExpression :: TokenParser AccessExpression
accessExpression = do
    (numericLiteral >>= return . DoubleAccessExpression)
    <|> (identifier >>= return . IdentAccessExpression)


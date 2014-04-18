module Parsing.Parser
(
    parseTokens,
    parseString
) where

import Control.Applicative ((<*))
import Text.ParserCombinators.Parsec (ParseError)
import Text.ParserCombinators.Parsec.Prim hiding (token)

import Lexing.Token
import Lexing.Lexer
import Parsing.Ast
import Parsing.TokenParser
import Parsing.Punctuator
import Parsing.Keyword

parseTokens :: [Token] -> Either ParseError Program
parseTokens input = runParser program () "tokens parser" input

parseString :: String -> Either ParseError Program
parseString input = case tokenize input of
    Left err -> Left err
    Right toks -> parseTokens toks

program :: TokenParser Program
program = many sourceElement >>= return . Program

sourceElement :: TokenParser SourceElement
sourceElement = statementSourceElement <|> functionDeclSourceElement

statementSourceElement :: TokenParser SourceElement
statementSourceElement = statement >>= return . StatementSourceElement

functionDeclSourceElement :: TokenParser SourceElement
functionDeclSourceElement = do
    name <- function >> identifier
    params <- parens $ sepByComma identifier
    body <- braces functionBody
    return $ FunctionDeclSourceElement name params body

functionBody :: TokenParser FunctionBody
functionBody = many sourceElement >>= return . FunctionBody

statement :: TokenParser Statement
statement = expressionStatement <|> varDeclStatement

expressionStatement :: TokenParser Statement
expressionStatement = do
    ass <- assignmentExpression <* semicolon
    return $ ExpressionStatement ass

assignmentExpression :: TokenParser AssignmentExpression
assignmentExpression = 
    try assignmentOperatorExpression
    <|> additiveAssignmentExpression

additiveAssignmentExpression :: TokenParser AssignmentExpression
additiveAssignmentExpression =
    additiveExpression >>= return . AdditiveAssignmentExpression

assignmentOperatorExpression :: TokenParser AssignmentExpression
assignmentOperatorExpression = do
    varName <- identifier
    expr <- assign >> additiveExpression
    return $ AssignmentOperatorExpression varName expr

varDeclStatement :: TokenParser Statement
varDeclStatement = do
    varName <- var >> identifier
    expr <- assign >> additiveExpression
    return $ VarDeclStatement varName expr

additiveExpression :: TokenParser AdditiveExpression
additiveExpression = do
    left <- unaryAdditiveExpression
    restOfAdditiveExpression left

--TODO: try to use 'chainl' combinator to deal with left recursion
restOfAdditiveExpression :: AdditiveExpression -> TokenParser AdditiveExpression
restOfAdditiveExpression left = do
    (try $ restOfPlusExpression left)
    <|> (try $ restOfMinusExpression left)
    <|> return left

restOfPlusExpression :: AdditiveExpression -> TokenParser AdditiveExpression
restOfPlusExpression left = do
    mult <- plus >> multExpression
    restOfAdditiveExpression $ PlusExpression left mult

restOfMinusExpression :: AdditiveExpression -> TokenParser AdditiveExpression
restOfMinusExpression left = do
    mult <- minus >> multExpression
    restOfAdditiveExpression $ MinusExpression left mult

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
    <|> return left

restOfMultMultExpression :: MultExpression -> TokenParser MultExpression
restOfMultMultExpression left = do
    acc <- multiplication >> accessExpression
    restOfMultExpression $ MultMultExpression left acc

restOfDivMultExpression :: MultExpression -> TokenParser MultExpression
restOfDivMultExpression left = do
    acc <- division >> accessExpression
    restOfMultExpression $ DivMultExpression left acc

accessExpression :: TokenParser AccessExpression
accessExpression = do
    doubleAccessExpression
    <|> try callAccessExpression
    <|> identifierAccessExpression

doubleAccessExpression :: TokenParser AccessExpression
doubleAccessExpression = numericLiteral >>= return . DoubleAccessExpression

identifierAccessExpression :: TokenParser AccessExpression
identifierAccessExpression = identifier >>= return . IdentAccessExpression

callAccessExpression :: TokenParser AccessExpression
callAccessExpression = do
    funName <- identifier
    params <- parens $ many assignmentExpression
    return $ CallAccessExpression funName params

module Parsing.Parser
(
    parseTokens,
    parseString
) where

import Text.ParserCombinators.Parsec (ParseError)
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Prim hiding (token)

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
expressionStatement = expression >>= return . ExpressionStatement

varDeclStatement :: TokenParser Statement
varDeclStatement = do
    keyword VarKeyword
    varName <- identifier
    punctuator AssignPunctuator
    expr <- expression
    return $ VarDeclStatement varName expr

expression :: TokenParser Expression
expression = do
    left <- unaryExpression
    restOfExpression left

--TODO: try to use 'chainl' combinator to deal with left recursion
restOfExpression :: Expression -> TokenParser Expression
restOfExpression left = do
    (try $ restOfPlusExpression left)
    <|> (try $ restOfMinusExpression left)
    <|> return left

restOfPlusExpression :: Expression -> TokenParser Expression
restOfPlusExpression left = do
    punctuator PlusPunctuator
    mult <- multExpression
    restOfExpression $ PlusExpression left mult

restOfMinusExpression :: Expression -> TokenParser Expression
restOfMinusExpression left = do
    punctuator MinusPunctuator
    mult <- multExpression
    restOfExpression $ MinusExpression left mult

unaryExpression :: TokenParser Expression
unaryExpression = multExpression >>= return . UnaryExpression

multExpression :: TokenParser MultExpression
multExpression = do
    left <- unaryMultExpression
    restOfMultExpression left

unaryMultExpression :: TokenParser MultExpression
unaryMultExpression = numericLiteral >>= return . UnaryMultExpression

restOfMultExpression :: MultExpression -> TokenParser MultExpression
restOfMultExpression left = do
    (try $ restOfMultMultExpression left)
    <|> (try $ restOfDivMultExpression left)
    <|> (return left)

restOfMultMultExpression :: MultExpression -> TokenParser MultExpression
restOfMultMultExpression left = do
    punctuator MultPunctuator
    num <- numericLiteral
    restOfMultExpression $ MultMultExpression left num

restOfDivMultExpression :: MultExpression -> TokenParser MultExpression
restOfDivMultExpression left = do
    punctuator DivPunctuator
    num <- numericLiteral
    restOfMultExpression $ DivMultExpression left num

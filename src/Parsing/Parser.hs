
module Parsing.Parser
(
    parseTokens,
    parseString
) where

import Control.Applicative ((<*))
import Text.ParserCombinators.Parsec (ParseError)
import Text.ParserCombinators.Parsec.Prim hiding (token)
import Text.ParserCombinators.Parsec.Combinator

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
program = many sourceElement <* eof >>= return . Program

sourceElement :: TokenParser SourceElement
sourceElement = statementSourceElement <|> functionDeclSourceElement

statementSourceElement :: TokenParser SourceElement
statementSourceElement = statement >>= return . StatementSourceElement

functionDeclSourceElement :: TokenParser SourceElement
functionDeclSourceElement = functionDeclaration >>= return . FunctionDeclSourceElement

functionDeclaration :: TokenParser FunctionDeclaration
functionDeclaration = do
    name <- function >> identifier
    params <- parens $ sepByComma identifier
    body <- braces $ many sourceElement
    return $ FunctionDeclaration name params body

statement :: TokenParser Statement
statement = 
        emptyStatement
    <|> blockStatement
    <|> expressionStatement
    <|> varDeclStatement
    <|> ifStatement
    <|> returnStatement
    <|> tryCatchStatement
    <|> throwStatement
    <?> "statement"

throwStatement :: TokenParser Statement
throwStatement = do
    expr <- throwKeyword >> logicalOrExpression
    return $ ThrowStatement expr

tryCatchStatement :: TokenParser Statement
tryCatchStatement = do
    tryBlock <- tryKeyword >> block
    ident <- catchKeyword >> parens identifier
    catchBlock <- block
    return $ TryCatchStatement tryBlock $ Catch ident catchBlock

block :: TokenParser Block
block = braces $ many statement

returnStatement :: TokenParser Statement
returnStatement = do
    returnKeyword
    expr <- optionMaybe logicalOrExpression
    semicolon
    return $ ReturnStatement expr

blockStatement :: TokenParser Statement
blockStatement = braces $ many statement >>= return . BlockStatement 

emptyStatement :: TokenParser Statement
emptyStatement = semicolon >> return EmptyStatement

ifStatement :: TokenParser Statement
ifStatement = do
    ifKeyword
    expr <- parens logicalOrExpression
    thenStmt <- statement
    elseStmt <- optionMaybe (elseKeyword >> statement)
    return $ IfStatement expr thenStmt elseStmt

expressionStatement :: TokenParser Statement
expressionStatement = do
    ass <- assignmentExpression <* semicolon
    return $ ExpressionStatement ass

assignmentExpression :: TokenParser AssignmentExpression
assignmentExpression = 
    try assignmentOperatorExpression
    <|> logicalAndAssignmentExpression

logicalAndAssignmentExpression :: TokenParser AssignmentExpression
logicalAndAssignmentExpression =
    logicalOrExpression >>= return . LogicalOrAssignmentExpression

assignmentOperatorExpression :: TokenParser AssignmentExpression
assignmentOperatorExpression = do
    varName <- identifier
    expr <- assign >> logicalOrExpression
    return $ AssignmentOperatorExpression varName expr

logicalOrExpression :: TokenParser LogicalOrExpression
logicalOrExpression = do
    left <- unaryLogicalOrExpression
    restOfLogicalOrExpression left

unaryLogicalOrExpression :: TokenParser LogicalOrExpression
unaryLogicalOrExpression = 
    logicalAndExpression >>= return . UnaryLogicalOrExpression

restOfLogicalOrExpression :: LogicalOrExpression -> TokenParser LogicalOrExpression
restOfLogicalOrExpression left = do
    (try $ nonEmptyRestOfLogicalOrExpression left)
    <|> return left

nonEmptyRestOfLogicalOrExpression :: LogicalOrExpression -> TokenParser LogicalOrExpression
nonEmptyRestOfLogicalOrExpression left = do
    expr <- logicalOr >> logicalAndExpression
    restOfLogicalOrExpression $ BinaryLogicalOrExpression left expr

logicalAndExpression :: TokenParser LogicalAndExpression
logicalAndExpression = do
    left <- unaryLogicalAndExpression
    restOfLogicalAndExpression left

unaryLogicalAndExpression :: TokenParser LogicalAndExpression
unaryLogicalAndExpression = equalityExpression >>= return . UnaryLogicalAndExpression

restOfLogicalAndExpression :: LogicalAndExpression -> TokenParser LogicalAndExpression
restOfLogicalAndExpression left = do
    (try $ nonEmptyRestOfLogicalAndExpression left)
    <|> return left

nonEmptyRestOfLogicalAndExpression :: LogicalAndExpression -> TokenParser LogicalAndExpression
nonEmptyRestOfLogicalAndExpression left = do
    expr <- logicalAnd >> equalityExpression
    restOfLogicalAndExpression $ BinaryLogicalAndExpression left expr

varDeclStatement :: TokenParser Statement
varDeclStatement = do
    varName <- var >> identifier
    expr <- assign >> additiveExpression
    return $ VarDeclStatement varName expr

equalityExpression :: TokenParser EqualityExpression
equalityExpression = do
    left <- unaryEqualityExpression
    restOfEqualityExpression left

restOfEqualityExpression :: EqualityExpression -> TokenParser EqualityExpression
restOfEqualityExpression left = do
    (try $ restOfEqualsExpression left)
    <|> (try $ restOfNotEqualsExpression left)
    <|> return left

restOfEqualsExpression :: EqualityExpression -> TokenParser EqualityExpression
restOfEqualsExpression left = do
    additive <- equals >> additiveExpression
    restOfEqualityExpression $ EqualsExpression left additive

restOfNotEqualsExpression :: EqualityExpression -> TokenParser EqualityExpression
restOfNotEqualsExpression left = do
    addititve <- notEquals >> additiveExpression
    restOfEqualityExpression $ NotEqualsExpression left addititve

unaryEqualityExpression :: TokenParser EqualityExpression
unaryEqualityExpression = additiveExpression >>= return . UnaryEqualityExpression

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
    <|> boolAccessExpression
    <|> try callAccessExpression
    <|> identifierAccessExpression
    <?> "access multExpression"

doubleAccessExpression :: TokenParser AccessExpression
doubleAccessExpression = numericLiteral >>= return . DoubleAccessExpression

boolAccessExpression :: TokenParser AccessExpression
boolAccessExpression = boolLiteral >>= return . BoolAccessExpression

identifierAccessExpression :: TokenParser AccessExpression
identifierAccessExpression = identifier >>= return . IdentAccessExpression

callAccessExpression :: TokenParser AccessExpression
callAccessExpression = do
    funName <- identifier
    params <- parens $ sepBy assignmentExpression comma
    return $ CallAccessExpression funName params

module Lexing.Identifier
(
    identifier
) where

import Text.ParserCombinators.Parsec

import Lexing.Token
import Lexing.Keyword (isKeyword)
import Lexing.BooleanLiteral (isBooleanLiteral)

identifier :: Parser Token
identifier = do
    str <- identifierStr
    if isReserved str 
        then fail "identifier can not be a keyword"
        else return $ IdentifierToken str

identifierStr :: Parser String
identifierStr = do
    first <- letter 
    rest <- many (letter <|> digit)
    return $ first:rest

isReserved :: String -> Bool
isReserved str = isKeyword str || isBooleanLiteral str

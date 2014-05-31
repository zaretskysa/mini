module Lexing.StringLiteral
(
    stringLiteral,
) where

import Text.ParserCombinators.Parsec

import Lexing.Token

stringLiteral :: Parser Token
stringLiteral = do
    char '"'
    str <- many stringCharacter
    char '"'
    return $ StringLiteralToken str

stringCharacter :: Parser Char
stringCharacter = do
    try $ notFollowedBy $ char '"'
    anyChar

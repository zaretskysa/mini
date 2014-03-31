module Lexing.NumericLiteral
(
    numericLiteral
) where

import Text.ParserCombinators.Parsec

import Lexing.Token

numericLiteral :: Parser Token
numericLiteral = do
    digits <- many1 digit
    return $ NumericLiteralToken $ read digits

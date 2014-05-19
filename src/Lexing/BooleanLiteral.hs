module Lexing.BooleanLiteral
(
    booleanLiteral,
    isBooleanLiteral
) where

import Text.ParserCombinators.Parsec

import Lexing.Token

booleanLiteral :: Parser Token
booleanLiteral = do
    value <- string "true" <|> string "false"
    return $ BooleanLiteral $ stringToBool value
    where 
        stringToBool "true" = True
        stringToBool _ = False

isBooleanLiteral :: String -> Bool
isBooleanLiteral str = str == "true" || str == "false"

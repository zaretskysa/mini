module Lexing.Keyword
(
    keyword,
    isKeyword
) where

import Text.ParserCombinators.Parsec

import Lexing.Token

keyword :: Parser Token
keyword = keyword' >>= return . KeywordToken 

keyword' :: Parser Keyword
keyword' = 
        (string "var" >> return VarKeyword)
    <|> (string "function" >> return FunctionKeyword)
    <|> (string "if" >> return IfKeyword)
    <|> (string "else" >> return ElseKeyword)

isKeyword :: String -> Bool
isKeyword str = elem str keywords
    where keywords = ["var", "function", "if", "else"]


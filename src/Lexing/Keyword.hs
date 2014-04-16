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
keyword' = var <|> function

var :: Parser Keyword
var = string "var" >> return VarKeyword

function :: Parser Keyword
function = string "function" >> return FunctionKeyword

isKeyword :: String -> Bool
isKeyword str = elem str keywords
    where keywords = ["var", "function"]


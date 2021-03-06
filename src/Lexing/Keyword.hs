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
    <|> (string "return" >> return ReturnKeyword)
    <|> try (string "try" >> return TryKeyword)
    <|> (string "catch" >> return CatchKeyword)
    <|> (string "throw" >> return ThrowKeyword)
    <|> (string "continue" >> return ContinueKeyword)
    <|> (string "break" >> return BreakKeyword)
    <|> (string "while" >> return WhileKeyword)

isKeyword :: String -> Bool
isKeyword str = elem str keywords
    where keywords = ["var", "function", "if", "else", "return", "try", "catch", "throw", "continue", "break", "while"]


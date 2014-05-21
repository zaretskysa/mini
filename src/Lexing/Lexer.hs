module Lexing.Lexer
(
    tokenize
)
where

import Text.ParserCombinators.Parsec hiding (tokens, token)

import Lexing.Token
import Lexing.NumericLiteral
import Lexing.BooleanLiteral
import Lexing.Identifier
import Lexing.Punctuator
import Lexing.Keyword

tokenize :: String -> Either ParseError [Token]
tokenize input = parse tokens "MiniTokenizer" input

tokens :: Parser [Token]
tokens = do
    toks <- sepBy token spaces
    eof
    return toks

token :: Parser Token
token = 
        numericLiteral
    <|> try identifier
    <|> booleanLiteral
    <|> keyword
    <|> punctuator

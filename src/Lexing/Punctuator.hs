module Lexing.Punctuator
(
    punctuator
) where

import Text.ParserCombinators.Parsec

import Lexing.Token

punctuator :: Parser Token
punctuator = punctuator' >>= return . PunctuatorToken

punctuator' :: Parser Punctuator
punctuator' = do
        plus
    <|> minus
    <|> mult
    <|> division
    <|> assign

plus :: Parser Punctuator
plus = char '+' >> return PlusPunctuator

minus :: Parser Punctuator
minus = char '-' >> return MinusPunctuator

mult :: Parser Punctuator
mult = char '*' >> return MultPunctuator

division :: Parser Punctuator
division = char '/' >> return DivPunctuator

assign :: Parser Punctuator
assign = char '=' >> return AssignPunctuator


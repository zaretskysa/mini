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
        try (string "==" >> return EqualsPunctuator)
    <|> try (string "!=" >> return NotEqualsPunctuator)
    <|> (string "&&" >> return LogicalAndPunctuator)
    <|> (string "||" >> return LogicalOrPunctuator)
    <|> (char '+' >> return PlusPunctuator)
    <|> (char '-' >> return MinusPunctuator)
    <|> (char '*' >> return MultPunctuator)
    <|> (char '/' >> return DivPunctuator)
    <|> (char '=' >> return AssignPunctuator)
    <|> (char ';' >> return SemicolonPunctuator)
    <|> (char ',' >> return CommaPunctuator)
    <|> (char '(' >> return OpenParenPunctuator)
    <|> (char ')' >> return CloseParenPunctuator)
    <|> (char '{' >> return OpenBracePunctuator)
    <|> (char '}' >> return CloseBracePunctuator)

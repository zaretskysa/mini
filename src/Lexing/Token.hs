module Lexing.Token
(
    Token(..),
    Punctuator(..),
    Keyword(..)
) where

data Token = 
      NumericLiteralToken Integer
    | IdentifierToken String
    | PunctuatorToken Punctuator
    | KeywordToken Keyword
    deriving (Show, Eq)

data Punctuator =
      PlusPunctuator
    | MinusPunctuator
    | MultPunctuator
    | DivPunctuator
    | AssignPunctuator
    deriving (Show, Eq)

data Keyword =
    VarKeyword
    deriving (Show, Eq)

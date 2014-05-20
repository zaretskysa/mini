module Lexing.Token
(
    Token(..),
    Punctuator(..),
    Keyword(..)
) where

data Token = 
      NumericLiteralToken Double
    | BooleanLiteral Bool
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
    | SemicolonPunctuator
    | CommaPunctuator
    | OpenParenPunctuator
    | CloseParenPunctuator
    | OpenBracePunctuator
    | CloseBracePunctuator
    | LogicalAndPunctuator
    | LogicalOrPunctuator
    deriving (Show, Eq)

data Keyword =
      VarKeyword
    | FunctionKeyword
    | IfKeyword
    | ElseKeyword
    | ReturnKeyword
    deriving (Show, Eq)

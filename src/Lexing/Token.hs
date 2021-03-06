module Lexing.Token
(
    Token(..),
    Punctuator(..),
    Keyword(..)
) where

data Token = 
      NumericLiteralToken Double
    | BooleanLiteralToken Bool
    | StringLiteralToken String
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
    | ColonPunctuator
    | SemicolonPunctuator
    | CommaPunctuator
    | OpenParenPunctuator
    | CloseParenPunctuator
    | OpenBracePunctuator
    | CloseBracePunctuator
    | LogicalAndPunctuator
    | LogicalOrPunctuator
    | EqualsPunctuator
    | NotEqualsPunctuator
    deriving (Show, Eq)

data Keyword =
      VarKeyword
    | FunctionKeyword
    | IfKeyword
    | ElseKeyword
    | ReturnKeyword
    | TryKeyword
    | CatchKeyword
    | ThrowKeyword
    | ContinueKeyword
    | BreakKeyword
    | WhileKeyword
    deriving (Show, Eq)

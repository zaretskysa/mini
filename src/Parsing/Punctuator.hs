module Parsing.Punctuator where

import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Prim ((<?>))

import Lexing.Token
import Parsing.TokenParser


assign :: TokenParser Punctuator
assign = punctuator AssignPunctuator <?> "assign"

semicolon :: TokenParser Punctuator
semicolon = punctuator SemicolonPunctuator <?> "semicolon"

comma :: TokenParser Punctuator
comma = punctuator CommaPunctuator <?> "comma"

openParen :: TokenParser Punctuator
openParen = punctuator OpenParenPunctuator <?> "open paren"

closeParen :: TokenParser Punctuator
closeParen = punctuator CloseParenPunctuator <?> "close paren"

parens :: TokenParser a -> TokenParser a
parens parser = between openParen closeParen parser

openBrace :: TokenParser Punctuator
openBrace = punctuator OpenBracePunctuator <?> "open brace"

closeBrace :: TokenParser Punctuator
closeBrace = punctuator CloseBracePunctuator <?> "close brace"

braces :: TokenParser a -> TokenParser a
braces parser = between openBrace closeBrace parser

plus :: TokenParser Punctuator
plus = punctuator PlusPunctuator <?> "plus"

minus :: TokenParser Punctuator
minus = punctuator MinusPunctuator <?> "minus"

multiplication :: TokenParser Punctuator
multiplication = punctuator MultPunctuator <?> "multiplication"

division :: TokenParser Punctuator
division = punctuator MultPunctuator <?> "division"

sepByComma :: TokenParser a -> TokenParser [a]
sepByComma parser = sepBy parser comma

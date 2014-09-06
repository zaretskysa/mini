module Parsing.Punctuator where

import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Prim ((<?>))

import Lexing.Token
import Parsing.TokenParser


assign :: TokenParser Punctuator
assign = punctuator AssignPunctuator <?> "="

colon :: TokenParser Punctuator
colon = punctuator ColonPunctuator <?> ":"

semicolon :: TokenParser Punctuator
semicolon = punctuator SemicolonPunctuator <?> ";"

comma :: TokenParser Punctuator
comma = punctuator CommaPunctuator <?> ","

openParen :: TokenParser Punctuator
openParen = punctuator OpenParenPunctuator <?> "("

closeParen :: TokenParser Punctuator
closeParen = punctuator CloseParenPunctuator <?> "("

parens :: TokenParser a -> TokenParser a
parens parser = between openParen closeParen parser

openBrace :: TokenParser Punctuator
openBrace = punctuator OpenBracePunctuator <?> "{"

closeBrace :: TokenParser Punctuator
closeBrace = punctuator CloseBracePunctuator <?> "}"

braces :: TokenParser a -> TokenParser a
braces parser = between openBrace closeBrace parser

plus :: TokenParser Punctuator
plus = punctuator PlusPunctuator <?> "+"

minus :: TokenParser Punctuator
minus = punctuator MinusPunctuator <?> "-"

multiplication :: TokenParser Punctuator
multiplication = punctuator MultPunctuator <?> "*"

division :: TokenParser Punctuator
division = punctuator MultPunctuator <?> "/"

logicalAnd :: TokenParser Punctuator
logicalAnd = punctuator LogicalAndPunctuator <?> "&&"

logicalOr :: TokenParser Punctuator
logicalOr = punctuator LogicalOrPunctuator <?> "||"

equals :: TokenParser Punctuator
equals = punctuator EqualsPunctuator <?> "=="

notEquals :: TokenParser Punctuator
notEquals = punctuator NotEqualsPunctuator <?> "!="

sepByComma :: TokenParser a -> TokenParser [a]
sepByComma parser = sepBy parser comma

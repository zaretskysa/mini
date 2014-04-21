module Parsing.Keyword where

import Text.ParserCombinators.Parsec.Prim ((<?>))

import Lexing.Token
import Parsing.TokenParser


var :: TokenParser Keyword
var = keyword VarKeyword <?> "var"

function :: TokenParser Keyword
function = keyword FunctionKeyword <?> "function"

ifKeyword :: TokenParser Keyword
ifKeyword = keyword IfKeyword <?> "if"

thenKeyword :: TokenParser Keyword
thenKeyword = keyword ElseKeyword <?> "then"

elseKeyword :: TokenParser Keyword
elseKeyword = keyword ElseKeyword <?> "else"


module Parsing.Keyword where

import Text.ParserCombinators.Parsec.Prim ((<?>))

import Lexing.Token
import Parsing.TokenParser


var :: TokenParser Keyword
var = keyword VarKeyword <?> "var"

function :: TokenParser Keyword
function = keyword FunctionKeyword <?> "function"

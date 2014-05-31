module Parsing.TokenParser
(
    TokenParser,
    anyToken,
    token,

    identifier,
    numericLiteral,
    boolLiteral,
    stringLiteral,
    anyPunctuator,
    punctuator,
    anyKeyword,
    keyword,
) where

import Text.ParserCombinators.Parsec.Combinator ()
import qualified Text.ParserCombinators.Parsec.Prim as P
import Text.ParserCombinators.Parsec.Prim hiding (token)
import Text.ParserCombinators.Parsec.Pos

import Lexing.Token

type TokenParser a = P.GenParser Token () a

anyToken :: TokenParser Token
anyToken = P.token showTok posFromTok testTok
    where
        showTok = show
        posFromTok _tok = newPos "" 0 0
        testTok t = Just t

--TODO: rewrite with 'satisfy' combinator
token :: Token -> TokenParser Token
token tok = do
    tok2 <- anyToken
    if tok == tok2 
        then return tok 
        else P.unexpected $ show tok2

identifier :: TokenParser String
identifier = try $ do
    tok <- anyToken
    case tok of
        IdentifierToken ident -> return ident
        _ -> fail "identified token"

numericLiteral :: TokenParser Double
numericLiteral = try $ do
    tok <- anyToken
    case tok of
        NumericLiteralToken num -> return num
        _ -> fail "numeric literal token"

boolLiteral :: TokenParser Bool
boolLiteral = try $ do
    tok <- anyToken
    case tok of
        BooleanLiteralToken val -> return val
        _ -> fail "boolean literal token"

stringLiteral :: TokenParser String
stringLiteral = try $ do
    tok <- anyToken
    case tok of
        StringLiteralToken val -> return val
        _ -> fail "string literal token"

anyPunctuator :: TokenParser Punctuator
anyPunctuator = try $ do
    tok <- anyToken
    case tok of
        PunctuatorToken punct -> return punct
        _ -> fail "punctuator token"

punctuator :: Punctuator -> TokenParser Punctuator
punctuator punct = try $ do
    punct2 <- anyPunctuator
    if punct == punct2
        then return punct
        else unexpected $ show punct2

anyKeyword :: TokenParser Keyword
anyKeyword = try $ do
    tok <- anyToken
    case tok of
        KeywordToken key -> return key
        _ -> fail "keyword token"

keyword :: Keyword -> TokenParser Keyword
keyword key = try $ do
    actual <- anyKeyword
    if key == actual
        then return key
        else unexpected $ show actual


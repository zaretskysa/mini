{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ParsingTests.ParsingTests
(
    htf_thisModulesTests,
) where

import Test.Framework

import Evaluating.Value
import Evaluating.Evaluator (evalString)


successfull source = successfullValue source UndefinedValue

successfullValue source expectedValue = assertEqual 
    (Right expectedValue)
    (evalString source)

exception source exceptionStr = assertEqual
    (Left $ StringValue exceptionStr)
    (evalString source)

parse_error source = exception source "parse_error"


test_emptyProgram = successfull ""

test_boolLiterals = successfull "true; false;"

test_numberLiterals = successfull "1; 2; 3;"

test_stringLiterals = successfull "\"hello\";"

test_whitespaceProgram = successfull " "

test_precedingWhitespace = successfull " true;"

test_followingWhitespace = successfull "true; "

test_precedingAndFollowingWhitespace = successfull " true; "

test_emptyStatement = successfull ";"

test_blockStatement = successfull "{}"

test_nestedBlockStatement = successfull "{{{{}{}}}}"

test_emptyReturnStatement = successfullValue "return;" UndefinedValue

test_nonEmptyReturnStatement = successfullValue "return 1+2;" $ NumberValue 3

test_simpleIfThenElseStatement = 
    successfullValue "if (true) return 1; else return 2;" $ NumberValue 1

test_nestedIfThenElseStatement = 
    successfullValue "if (true) if (true) return 1; else return 2;" $ NumberValue 1




{-# OPTIONS_GHC -F -pgmF htfpp #-}

module EvaluatingTests.ExceptionTests
(
    htf_thisModulesTests,
) where

import Test.Framework

import Evaluating.Value
import Evaluating.Evaluator (evalString)


test_noBindingException = assertEqual
    (Left $ StringValue "no_binding_x")
    (evalString "return x;")

test_notFunctionException = assertEqual
    (Left $ StringValue "not_function_x")
    (evalString "x = 1; return x();")

test_parseErrorException = assertEqual
    (Left $ StringValue "parse_error")
    (evalString "if();")

test_simpleThrow = assertEqual
    (Left $ NumberValue 33)
    (evalString "throw 33;")

test_simpleTryCatchWithoutThrow = assertEqual
    (Right $ NumberValue 11)
    (evalString "try { return 11; } catch (e) {}")

test_simpleCatch = assertEqual
    (Right $ NumberValue 22)
    (evalString "try { throw 11; } catch (e) {return 22;}")

test_accessCatchedException = assertEqual
    (Right $ NumberValue 22)
    (evalString "try { throw 11; } catch (e) {return 2*e;}")

test_rebindVariableInsideCatchBlock = assertEqual
    (Right $ NumberValue 2)
    (evalString "e = 3; try { throw 1; } catch (e) {return 2*e;}")

test_accessOutsideVariableFromCatchBlock = assertEqual
    (Right $ NumberValue 6)
    (evalString "e = 3; try { throw 1; } catch (x) {return 2*e;}")

test_rethrowException = assertEqual
    (Left $ NumberValue 2)
    (evalString "try { throw 1; } catch (e) {throw 2;}")

test_nestedTryCatch = assertEqual
    (Right $ NumberValue 6)
    (evalString "try { try { throw 1; } catch (e) { throw e*2; } } catch (e) { return e*3; }")

test_throwFromNestedBlock = assertEqual
    (Right $ NumberValue 1)
    (evalString "try { { throw 1; } } catch (e) {return e;}")

test_uncaughtExceptionGereratedInsideFunction = assertEqual
    (Left $ NumberValue 1)
    (evalString "function foo(){throw 1;} return foo();")

test_restoringStackAfterExceptionGeneratedByFunctionCall = assertEqual
    (Left $ StringValue "no_binding_x")
    (evalString "function foo(){x = 2; throw 1;} try {foo();} catch (e) {x;}")

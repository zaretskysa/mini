{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import {-@ HTF_TESTS @-} EvaluatingTests.ExceptionTests
import {-@ HTF_TESTS @-} ParsingTests.ParsingTests

main :: IO ()
main = htfMain htf_importedTests

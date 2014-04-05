import System.Environment

import Text.Show.Pretty

import Lexing.Lexer
import Parsing.Parser
import Evaluating.Evaluator

main :: IO ()
main = do
    args <- getArgs
    if null args
        then putStrLn "No input provided"
        else do
            putStrLn $ ppShow $ tokenize $ head args
            putStrLn $ ppShow $ parseString $ head args
            putStrLn $ ppShow $ evalString $ head args

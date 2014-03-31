import System.Environment

import Text.Show.Pretty

import Lexing.Lexer

main :: IO ()
main = do
    args <- getArgs
    if null args
        then putStrLn "No input provided"
        else do
            putStrLn $ ppShow $ tokenize $ head args

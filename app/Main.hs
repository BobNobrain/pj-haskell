module Main where

import Lib
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    mapM_ putStrLn args

parseArgs :: [String] -> IO ()

parseArgs ("add":name:path:[])          = putStrLn "Add invocation"
parseArgs ("add":[])                    = parseArgs ("add":"--help0":[])
parseArgs ("add":"help":[])             = parseArgs ("add":"--help":[])
parseArgs ("add":"--help":[])           = putStrLn "Add help invocation"
parseArgs ("add":_)                     = putStrLn "Incorrect add invocation"

parseArgs ("rm":"--help":[])            = putStrLn "Rm help invocation"
parseArgs ("rm":names)                  = putStrLn "Rm invocation"
parseArgs ("rm":_)                      = putStrLn "Incorrect rm invocation"

parseArgs ("list":[])                   = putStrLn "List invocation"
parseArgs ("list":"--help":[])          = putStrLn "List help invocation"
parseArgs ("list":"help":[])            = parseArgs ("list":"--help":[])

parseArgs ("--help":[])                 = putStrLn "Help invocation"

parseArgs (name:[])                     = putStrLn "Get invocation"

parseArgs _                             = putStrLn "Incorrect invocation at all"

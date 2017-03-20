module Main where

import PjFunc
import PjHelp
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    mapM_ putStrLn args

parseArgs :: [String] -> IO ()

parseArgs ("add":name:path:[])          = pjadd name path
parseArgs ("add":[])                    = parseArgs ("add":"--help0":[])
parseArgs ("add":"help":[])             = parseArgs ("add":"--help":[])
parseArgs ("add":"--help":[])           = putStrLn $ pjhelp ["add"]
parseArgs ("add":_)                     = pjinc ["add"]

parseArgs ("rm":"--help":[])            = putStrLn $ pjhelp ["rm"]
parseArgs ("rm":names)                  = pjrm names
parseArgs ("rm":_)                      = pjinc ["rm"]

parseArgs ("list":[])                   = pjlist
parseArgs ("list":"--help":[])          = putStrLn $ pjhelp ["list"]
parseArgs ("list":"help":[])            = parseArgs ("list":"--help":[])

parseArgs ("--help":[])                 = putStrLn $ pjhelp []

parseArgs (name:[])                     = pjget name

parseArgs _                             = pjinc []

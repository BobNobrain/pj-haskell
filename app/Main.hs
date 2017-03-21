module Main where

import PjFunc
import PjHelp
import PjErr
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    parseArgs args

parseArgs :: [String] -> IO ()

-- TODO: remove pjinc
parseArgs ("add":name:path:[])          = execute $ pjadd name path
parseArgs ("add":[])                    = parseArgs ("add":"--help":[])
parseArgs ("add":"help":[])             = parseArgs ("add":"--help":[])
parseArgs ("add":"--help":[])           = printHelp "add"
parseArgs ("add":_)                     = pjinc ["add"]

parseArgs ("rm":"--help":[])            = printHelp "rm"
parseArgs ("rm":names)                  = execute $ pjrm names
parseArgs ("rm":_)                      = pjinc ["rm"]

parseArgs ("list":[])                   = pjlist
parseArgs ("list":"--help":[])          = printHelp "list"
parseArgs ("list":"help":[])            = parseArgs ("list":"--help":[])

parseArgs ("--help":[])                 = printHelp ""

parseArgs (name:[])                     = execute $ pjget name

parseArgs _                             = pjinc []

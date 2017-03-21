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

parseArgs ("add":name:path:[])          = execute $ pjadd name path
parseArgs ("add":[])                    = parseArgs ("add":"--help":[])
parseArgs ("add":"help":[])             = parseArgs ("add":"--help":[])
parseArgs ("add":"--help":[])           = printHelp "add"
parseArgs ("add":_)                     = execInvCmd "add"

parseArgs ("rm":"--help":[])            = printHelp "rm"
parseArgs ("rm":n:ns)                   = execute $ pjrm (n:ns)
parseArgs ("rm":_)                      = execInvCmd "rm"

parseArgs ("list":[])                   = execute pjlist
parseArgs ("list":"--help":[])          = printHelp "list"
parseArgs ("list":"help":[])            = parseArgs ("list":"--help":[])

parseArgs ("--help":[])                 = printHelp ""

parseArgs (name:[])                     = execute $ pjget name

parseArgs _                             = execInvCmd ""

module PjHelp
    ( pjhelp
    , pjinc
    ) where

pjhelp :: [String] -> String
pjhelp _ = "Help not done yet"

pjinc :: [String] -> IO ()
pjinc [cmd] = do
    putStrLn $ "Incorrect usage of " ++ cmd
    putStrLn $ pjhelp [cmd]

pjinc [] = do
    putStrLn "Incorrect usage"
    putStrLn $ pjhelp []

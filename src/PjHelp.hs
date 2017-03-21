module PjHelp
    ( pjhelp
    , printHelp
    ) where

pjhelp :: String -> [String]
pjhelp _ = ["Help not done yet", "Please, stand by"]

printHelp :: String -> IO ()
printHelp cmd = mapM_ putStrLn $ pjhelp cmd

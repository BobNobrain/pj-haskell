module PjHelp
    ( pjhelp
    , pjinc
    , printHelp
    ) where

pjhelp :: String -> [String]
pjhelp _ = ["Help not done yet", "Please, stand by"]

pjinc :: [String] -> IO ()
pjinc [cmd] = do
    putStrLn $ "Incorrect usage of " ++ cmd
    mapM_ putStrLn $ pjhelp cmd

pjinc [] = do
    putStrLn "Incorrect usage"
    mapM_ putStrLn $ pjhelp ""

printHelp :: String -> IO ()
printHelp cmd = mapM_ putStrLn $ pjhelp cmd

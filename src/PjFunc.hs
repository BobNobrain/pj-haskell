module PjFunc
    ( pjget
    , pjadd
    , pjrm
    , pjlist
    ) where

-- reads a single entry from pj file and prints it
pjget :: String -> IO ()
pjget name = putStrLn $ "pj-get " ++ name

-- adds a single entry into pj file
pjadd :: String -> String -> IO ()
pjadd name path = putStrLn $ "pj-add " ++ name ++ " at " ++ path

-- removes one or more entries from pj file
pjrm :: [String] -> IO ()
pjrm names = putStrLn $ "pj-rm " ++ (unwords names)

-- lists the pj file entries
pjlist :: IO ()
pjlist = putStrLn "I am the list"

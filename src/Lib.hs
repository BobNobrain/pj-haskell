module Lib
    ( someFunc
    , pjget
    , pjadd
    , pjrm
    , pjlist
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

pjget :: String -> IO ()
pjget name = putStrLn $ "pj-get " ++ name

pjadd :: String -> String -> IO ()
pjadd name path = putStrLn $ "pj-add " ++ name ++ " at " ++ path

pjrm :: String -> IO ()
pjrm name = putStrLn $ "pj-rm " ++ name

pjlist :: IO ()
pjlist = putStrLn "I am the list"
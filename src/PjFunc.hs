module PjFunc
    ( pjget
    , pjadd
    , pjrm
    , pjlist
    ) where

import System.Path.NameManip (guess_dotdot, absolute_path)
import System.FilePath (addTrailingPathSeparator, normalise)
import System.Directory (getHomeDirectory, doesFileExist)
import System.IO hiding (readFile)
import Data.Maybe (fromJust)
import Data.List (isPrefixOf)

-- https://www.schoolofhaskell.com/user/dshevchenko/cookbook/transform-relative-path-to-an-absolute-path
absolutize :: String -> IO String
absolutize aPath 
    | "~" `isPrefixOf` aPath = do
        homePath <- getHomeDirectory
        return $ normalise $ addTrailingPathSeparator homePath 
                             ++ tail aPath
    | otherwise = do
        pathMaybeWithDots <- absolute_path aPath
        return $ fromJust $ guess_dotdot pathMaybeWithDots


filepath = "~/.pj"

openConfigReadWrite :: IO Handle
openConfigReadWrite = do
    path <- absolutize filepath
    openFile path ReadWriteMode

openConfigWrite :: IO Handle
openConfigWrite = do
    path <- absolutize filepath
    openFile path AppendMode


-- reads a single entry from pj file and prints it
pjget :: String -> IO ()
pjget name = do
    if validateName name then
        putStrLn $ "pj-get " ++ name
    else
        putStrLn "Invalid project name given"

-- adds a single entry into pj file
pjadd :: String -> String -> IO ()
pjadd name path =
    if validateName name then do
        fileH <- openConfigWrite
        aPath <- absolutize path
        hPutStrLn fileH (name ++ " " ++ aPath)
        hClose fileH
        putStrLn $ "Added '" ++ name ++ "' project at path '" ++ path ++ "'"
        putStrLn $ "(absolute is '" ++ aPath ++ "')"
    else do
        putStrLn $ "Invalid project name '" ++ name ++ "'"
        putStrLn "Project name should not contain whitespace"


-- removes one or more entries from pj file
pjrm :: [String] -> IO ()
pjrm names = putStrLn $ "pj-rm " ++ (unwords names)

-- lists the pj file entries
pjlist :: IO ()
pjlist = openConfigReadWrite >>= loopFile' >>= showCount
    where
        loopFile' :: Handle -> IO Int
        loopFile' h = loopFile h putStrLn

        showCount :: Int -> IO ()
        showCount n = putStrLn $ "Total entries: " ++ (show n)

loopFile :: Handle -> (String -> IO ()) -> IO Int
loopFile = loop 0
    where
        loop :: Int -> Handle -> (String -> IO ()) -> IO Int
        loop n fileH callback = do
            isEOF <- hIsEOF fileH
            if isEOF then do
                hClose fileH
                return n
            else do
                str <- hGetLine fileH
                callback str
                loop (n + 1) fileH callback
        


validateName :: String -> Bool
validateName "" = False
validateName str = validateName' str 
    where
        validateName' :: String -> Bool
        validateName' (' ':xs) = False
        validateName' (x:xs) = validateName' xs
        validateName' "" = True

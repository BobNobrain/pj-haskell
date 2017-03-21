module PjFunc
    ( pjget
    , pjadd
    , pjrm
    , pjlist
    ) where

import System.Path.NameManip (guess_dotdot, absolute_path)
import System.FilePath (addTrailingPathSeparator, normalise)
import System.Directory (getHomeDirectory, doesFileExist, copyFile, removeFile)
import System.IO hiding (readFile)
import System.Exit
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
tempFilepath = "~/.pj.temp"

openConfig :: IOMode -> IO Handle
openConfig m = do
    path <- absolutize filepath
    openFile path m

data Modification
    = Write String
    | Skip
    | WriteUnmodified String

type Modifier = String -> Modification


-- reads a single entry from pj file and prints it
pjget :: String -> IO ()
pjget name = do
    if validateName name then
        let
            callback :: String -> String -> IO ()
            callback name str
                | name == str_name = putStrLn str_path
                | otherwise = return ()
                    where
                        w = words str
                        str_name = head w
                        str_path = unwords $ tail w
        in do
            fileH <- openConfig ReadMode
            n <- loopFile fileH $ callback name
            return ()
    else
        putStrLn "Invalid project name given"

-- adds a single entry into pj file
pjadd :: String -> String -> IO ()
pjadd name path =
    if validateName name then do
        -- check if we are modifying existing entry or adding new one
        aPath <- absolutize path
        let
            modifier :: String -> String -> Modifier
            modifier name path str
                | head (words str) == name = Write (name ++ " " ++ path)
                | otherwise = WriteUnmodified str
        modifiedCount <- modifyConfig $ modifier name aPath

        if modifiedCount == 0 then do
            -- there were no previous entry about $name
            fileH <- openConfig AppendMode
            hPutStrLn fileH (name ++ " " ++ aPath)
            hClose fileH
            putStrLn $ "Added '" ++ name ++ "' project at path '" ++ path ++ "'"
            putStrLn $ "(absolute is '" ++ aPath ++ "')"
        else do
            -- there was an entry, and it was modified
            putStrLn $ "Changed '" ++ name ++ "' project to path '" ++ path ++ "'"
            putStrLn $ "(absolute is '" ++ aPath ++ "')"
    else do
        putStrLn $ "Invalid project name '" ++ name ++ "'"
        putStrLn "Project name should not contain whitespace"


-- removes one or more entries from pj file
pjrm :: [String] -> IO ()
pjrm names = do
    n <- modifyConfig $ modifier names
    putStrLn $ "Removed entries: " ++ (show n)
        where
            modifier :: [String] -> Modifier
            modifier [] str = WriteUnmodified str
            modifier names str = if name `elem` names then Skip else WriteUnmodified str
                where
                    name = head $ words str

-- lists the pj file entries
pjlist :: IO ()
pjlist = (openConfig ReadWriteMode) >>= loopFile' >>= showCount
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


modifyConfig :: Modifier -> IO Int
modifyConfig modifier = do
    -- cp ~/.pj ~/.pj.temp
    pjTemp <- absolutize tempFilepath
    pjFile <- absolutize filepath
    copyFile pjFile pjTemp
    -- preparing handles
    tempFileH <- openFile pjTemp ReadMode
    pjFileH <- openFile pjFile WriteMode
    -- main loop here: line-by-line reading and writing
    n <- loop 0 tempFileH pjFileH modifier
    -- resource freeing
    hClose tempFileH
    hClose pjFileH
    -- cleanup
    removeFile pjTemp
    return n
        where
            loop :: Int -> Handle -> Handle -> Modifier -> IO Int
            loop modifiedCount fileR fileW modifier = do
                isEof <- hIsEOF fileR
                if isEof then
                    return modifiedCount
                else do
                    readStr <- hGetLine fileR
                    n <- modify modifiedCount fileW $ modifier readStr
                    loop n fileR fileW modifier
                        where
                            modify :: Int -> Handle -> Modification -> IO Int
                            modify n fileW Skip = return (n + 1)
                            modify n fileW (WriteUnmodified s) = do
                                hPutStrLn fileW s
                                return n
                            modify n fileW (Write s) = do
                                hPutStrLn fileW s
                                return (n + 1)

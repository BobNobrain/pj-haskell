module PjFunc
    ( pjget
    , pjadd
    , pjrm
    , pjlist
    ) where

import System.Path.NameManip (guess_dotdot, absolute_path)
import System.FilePath (addTrailingPathSeparator, normalise)
import System.Directory (getHomeDirectory, copyFile, removeFile, doesDirectoryExist)
import System.IO
import Data.Maybe (fromJust)
import Data.List (isPrefixOf)

import PjErr

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
pjget :: String -> IO OperationResult
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
            return $ if n == 0 then OpNoEntry name else OpSuccess
    else
        return $ OpInvalidName name

-- adds a single entry into pj file
pjadd :: String -> String -> IO OperationResult
pjadd name path =
    if validateName name then do
        -- check if we are modifying existing entry or adding new one
        aPath <- absolutize path
        isDir <- doesDirectoryExist aPath
        if not isDir then
            putStrLn "Warning: specified path is not a directory!"
        else
            return ()
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
            return $ OpSuccessMsg [ "Added '" ++ name ++ "' project at path '" ++ path ++ "'"
                                  , "(absolute is '" ++ aPath ++ "')"
                                  ]
        else
            -- there was an entry, and it was modified
            return $ OpSuccessMsg [ "Changed '" ++ name ++ "' project to path '" ++ path ++ "'"
                                  , "(absolute is '" ++ aPath ++ "')"
                                  ]
    else
        return $ OpInvalidName name


-- removes one or more entries from pj file
pjrm :: [String] -> IO OperationResult
pjrm names = do
    n <- modifyConfig $ modifier names
    return $ OpSuccessMsg ["Removed entries: " ++ (show n)]
        where
            modifier :: [String] -> Modifier
            modifier [] str = WriteUnmodified str
            modifier names str = if name `elem` names then Skip else WriteUnmodified str
                where
                    name = head $ words str

-- lists the pj file entries
pjlist :: IO OperationResult
pjlist = (outputEntry "NAME PATH") >> (openConfig ReadWriteMode) >>= loopFile' >>= showCount
    where
        loopFile' :: Handle -> IO Int
        loopFile' h = loopFile h outputEntry

        showCount :: Int -> IO OperationResult
        showCount n = return $ OpSuccessMsg ["  Total entries: " ++ (show n)]

        outputEntry :: String -> IO ()
        outputEntry str = putStrLn $ beautifiedName ++ " " ++ path
            where
                w = words str
                name = head w
                path = unwords $ tail w
                -- i assume that 20 chars for name will be quite enough
                pad = 20 - (length name)
                padP = if pad < 0 then 0 else pad 
                beautifiedName = name ++ (replicate padP ' ')

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

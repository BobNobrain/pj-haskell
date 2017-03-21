module PjErr
    ( OperationError
    , getExitCode
    , execute
    ) where

import System.Exit
import PjHelp

data OperationError
    = OpSuccess
    | OpInvalidName String
    | OpNoEntry String
    | OpInvalidCommand String

getExitCode :: OperationError -> IO ExitCode

getExitCode OpSuccess = return ExitSuccess

getExitCode (OpInvalidName name) = do
    putStrLn $ "Invalid project name '" ++ name ++ "'"
    putStrLn "Project name should not contain whitespace"
    return $ ExitFailure 4

getExitCode (OpNoEntry name) = do
    putStrLn $ "No entry with name '" ++ name ++ "' found"
    return $ ExitFailure 8

getExitCode (OpInvalidCommand cmd) = do
    putStrLn $ if cmd == "" then "Incorrect usage" else "Incorrect usage of " ++ cmd
    mapM_ putStrLn $ pjhelp cmd
    return $ ExitFailure 16

execute :: IO OperationError -> IO ()
execute f = f >>= getExitCode >>= exitWith

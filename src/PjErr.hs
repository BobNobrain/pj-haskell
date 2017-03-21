module PjErr
    ( OperationResult (..)
    , getExitCode
    , execute
    , execInvCmd
    ) where

import System.Exit
import PjHelp

data OperationResult
    = OpSuccess
    | OpSuccessMsg [String]
    | OpInvalidName String
    | OpNoEntry String
    | OpInvalidCommand String

getExitCode :: OperationResult -> IO ExitCode

getExitCode OpSuccess = return ExitSuccess

getExitCode (OpSuccessMsg lines) = mapM_ putStrLn lines >> return ExitSuccess

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

execute :: IO OperationResult -> IO ()
execute f = f >>= getExitCode >>= exitWith

execInvCmd :: String -> IO ()
execInvCmd cmd = getExitCode (OpInvalidCommand cmd) >>= exitWith

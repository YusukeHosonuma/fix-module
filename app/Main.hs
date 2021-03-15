{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Exception
import           Control.Monad
import           FixModule
import           System.Directory
import           System.Environment
import           System.Exit        (exitFailure, exitSuccess)

main :: IO ()
main = do
    args <- getArgs
    when ("--version" `elem` args) printVersion
    exist <- doesFileExist "package.yaml"
    unless exist exitPackageYamlNotFound
    pwd <- getCurrentDirectory
    fixModule pwd `catch` reportException

printVersion :: IO ()
printVersion = do
    putStrLn "fix-module 0.1.0"
    exitSuccess

exitPackageYamlNotFound :: IO ()
exitPackageYamlNotFound = do
    putStrLn "A package.yaml is not found. (Not support .cabal yet)"
    exitFailure

reportException :: SomeException -> IO ()
reportException e = do
    putStrLn "Fatal error.\n"
    putStrLn $ "[Cause]\n" ++ displayException e ++ "\n"
    putStrLn "Please report issue when reproducible."
    putStrLn "https://github.com/YusukeHosonuma/fix-module/issues/new"

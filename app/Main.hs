module Main where

import           Control.Monad
import           FixModule
import           System.Directory
import           System.Environment
import           System.Exit

main :: IO ()
main = do
    args <- getArgs
    when ("--version" `elem` args) printVersion
    exist <- doesFileExist "package.yaml"
    unless exist exitPackageYamlNotFound
    pwd <- getCurrentDirectory
    fixModule pwd  -- TODO: 例外処理

printVersion :: IO ()
printVersion = do
    putStrLn "fix-module 0.1.0.0"
    exitSuccess

exitPackageYamlNotFound :: IO ()
exitPackageYamlNotFound = do
    putStrLn "package.yaml is not found. (not support .cabal yet)"
    exitFailure

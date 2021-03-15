{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import System.Process
import FixModule.Module
import Control.Exception
import Control.Monad.Trans.Reader

main :: IO ()
main = defaultMain $
    testCase "fixModule" $ do
        runReaderT (fixModule "./test-asset") $ Env True -- enable `--verbose `
        callCommand "stack build --stack-yaml ./test-asset/stack.yaml"
            `catch`   (\(e :: SomeException) -> assertFailure $ displayException e)
            `finally` callCommand "git checkout HEAD ./test-asset" -- clean-up

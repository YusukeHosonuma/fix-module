{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import System.Process
import FixModule
import Control.Exception

main :: IO ()
main = defaultMain $
    testCase "fixModule" $ do
        fixModule "./test-asset"
        callCommand "stack build --stack-yaml ./test-asset/stack.yaml"
            `catch`   (\(e :: SomeException) -> assertFailure $ displayException e)
            `finally` callCommand "git checkout HEAD ./test-asset" -- clean-up

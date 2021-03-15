{-# LANGUAGE OverloadedStrings #-}

module FixModule.Package (lookupSourceDirs) where

import qualified Data.HashMap.Strict as S
import qualified Data.Text           as T
import           Data.Yaml

lookupSourceDirs :: IO [String]
lookupSourceDirs = do
    yaml <- decodeFileThrow "package.yaml" :: IO Value
    return $ map T.unpack $ findSourceDirs yaml

findSourceDirs :: Value -> [T.Text]
findSourceDirs (Object o) = S.foldlWithKey f [] o
    where
        f :: [T.Text] -> T.Text -> Value -> [T.Text]
        f a "source-dirs" (String v) = v : a
        f a _ v                      = a ++ findSourceDirs v
findSourceDirs _ = []

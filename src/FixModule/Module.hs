module FixModule.Module (fixModuleRecursive)  where

import           Control.Monad
import           Data.List
import           System.Directory
import           System.FilePath.Posix
import           System.IO.Extra

type ModuleName = String

fixModuleRecursive :: FilePath -> IO ()
fixModuleRecursive rootDir = do
    ps <- findHaskellFilePathes rootDir
    mapM_ (fixModuleFile rootDir) ps

--------------------------------------------------------------------------------

findHaskellFilePathes :: FilePath -> IO [FilePath]
findHaskellFilePathes path = do
    xs <- map (path </>) . filter (`notElem` [".", ".."]) <$> getDirectoryContents path
    let hsFiles = filter (".hs" `isExtensionOf`) xs
    dirs <- filterM doesDirectoryExist xs
    subHsFiles <- concat <$> mapM findHaskellFilePathes dirs
    return $ hsFiles ++ subHsFiles

fixModuleFile :: FilePath -> FilePath -> IO ()
fixModuleFile rootDir target = do
    let mName = moduleName rootDir target
    valid <- isValidModule mName target
    if valid
        then
            putStrLn $ "Skip: " ++ target -- verbose に対応
        else do
            updateFileWith target (fixModuleContent mName)
            putStrLn $ "Done: " ++ target

fixModuleContent :: ModuleName -> String -> String
fixModuleContent mName = unlines . map (fixModuleLine mName) . lines

-- |
-- >>> isValidModule "Foo.Bar.Fuga" "./test-asset/src/Foo/Bar/Fuga.hs"
-- False
-- >>> isValidModule "Foo.Bar.Valid" "./test-asset/src/Foo/Bar/Valid.hs"
-- True
--
isValidModule :: ModuleName -> FilePath -> IO Bool
isValidModule mName path = withFile path ReadMode loop
    where
        loop :: Handle -> IO Bool
        loop hdl = do
            eof <- hIsEOF hdl
            if eof
                then return False
                else do
                    s <- hGetLine hdl
                    case words s of
                        ("module":x:_) -> return $ x == mName
                        _              -> loop hdl

-- |
-- >>> fixModuleLine "Lib" "module Foo where"
-- "module Lib where"
-- >>> fixModuleLine "Lib" " module Foo (foo, bar) where "
-- "module Lib (foo, bar) where"
--
fixModuleLine :: ModuleName -> String -> String
fixModuleLine mName line =
    case words line of
        ("module":_:xs) -> "module " ++ mName ++ " " ++ unwords xs
        _               -> line

-- |
-- >>> moduleName "./src" "./src/Lib.hs"
-- "Lib"
-- >>> moduleName "./src" "./src/App/Lib.hs"
-- "App.Lib"
-- >>> moduleName "./src" "./src/App/Snd/Lib.hs"
-- "App.Snd.Lib"
--
moduleName :: String -> FilePath -> String
moduleName root filePath =
    intercalate "." . splitDirectories . dropExtensions $ modulePath
        where
            modulePath = tail $ drop (length root) filePath

--------------------------------------------------------------------------------

updateFileWith :: FilePath -> (String -> String) -> IO ()
updateFileWith fp f = do
    contents <- readFile' fp
    writeFile fp $ f contents

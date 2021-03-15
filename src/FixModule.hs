module FixModule (fixModule) where

import           FixModule.Module
import           FixModule.Package
import           System.FilePath.Posix

fixModule :: FilePath -> IO ()
fixModule path = do
    dirs <- map (path </>) <$> lookupSourceDirs
    mapM_ fixModuleRecursive dirs

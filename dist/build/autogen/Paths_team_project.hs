module Paths_team_project (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/notenot/.cabal/bin"
libdir     = "/home/notenot/.cabal/lib/x86_64-linux-ghc-7.10.3/team-project-0.0.0-CiA0Ec4fhpE6aYB4RAT0xT"
datadir    = "/home/notenot/.cabal/share/x86_64-linux-ghc-7.10.3/team-project-0.0.0"
libexecdir = "/home/notenot/.cabal/libexec"
sysconfdir = "/home/notenot/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "team_project_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "team_project_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "team_project_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "team_project_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "team_project_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

module Paths_servermon (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/blischalk/haskell/servermon/.stack-work/install/x86_64-osx/lts-3.17/7.10.2/bin"
libdir     = "/Users/blischalk/haskell/servermon/.stack-work/install/x86_64-osx/lts-3.17/7.10.2/lib/x86_64-osx-ghc-7.10.2/servermon-0.1.0.0-BzRxO665O4xKUS1eyxfvLk"
datadir    = "/Users/blischalk/haskell/servermon/.stack-work/install/x86_64-osx/lts-3.17/7.10.2/share/x86_64-osx-ghc-7.10.2/servermon-0.1.0.0"
libexecdir = "/Users/blischalk/haskell/servermon/.stack-work/install/x86_64-osx/lts-3.17/7.10.2/libexec"
sysconfdir = "/Users/blischalk/haskell/servermon/.stack-work/install/x86_64-osx/lts-3.17/7.10.2/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "servermon_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "servermon_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "servermon_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "servermon_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "servermon_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

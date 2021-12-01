{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_nixtest (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/ent/.cabal/bin"
libdir     = "/home/ent/.cabal/lib/x86_64-linux-ghc-8.8.4/nixtest-0.1.0.0-inplace-nixtest"
dynlibdir  = "/home/ent/.cabal/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/ent/.cabal/share/x86_64-linux-ghc-8.8.4/nixtest-0.1.0.0"
libexecdir = "/home/ent/.cabal/libexec/x86_64-linux-ghc-8.8.4/nixtest-0.1.0.0"
sysconfdir = "/home/ent/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "nixtest_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "nixtest_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "nixtest_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "nixtest_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "nixtest_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "nixtest_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

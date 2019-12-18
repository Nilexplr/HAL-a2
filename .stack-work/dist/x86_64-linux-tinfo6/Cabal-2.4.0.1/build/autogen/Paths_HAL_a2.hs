{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_HAL_a2 (
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

bindir     = "/app/.stack-work/install/x86_64-linux-tinfo6/dedd53dcbe9a53bc94c4f9e1e6597e15d4d7393d570b4a26f9e6bcdce0b91d81/8.6.5/bin"
libdir     = "/app/.stack-work/install/x86_64-linux-tinfo6/dedd53dcbe9a53bc94c4f9e1e6597e15d4d7393d570b4a26f9e6bcdce0b91d81/8.6.5/lib/x86_64-linux-ghc-8.6.5/HAL-a2-0.1.0.0-IY4lNA441FT58uUmbE7TIA"
dynlibdir  = "/app/.stack-work/install/x86_64-linux-tinfo6/dedd53dcbe9a53bc94c4f9e1e6597e15d4d7393d570b4a26f9e6bcdce0b91d81/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/app/.stack-work/install/x86_64-linux-tinfo6/dedd53dcbe9a53bc94c4f9e1e6597e15d4d7393d570b4a26f9e6bcdce0b91d81/8.6.5/share/x86_64-linux-ghc-8.6.5/HAL-a2-0.1.0.0"
libexecdir = "/app/.stack-work/install/x86_64-linux-tinfo6/dedd53dcbe9a53bc94c4f9e1e6597e15d4d7393d570b4a26f9e6bcdce0b91d81/8.6.5/libexec/x86_64-linux-ghc-8.6.5/HAL-a2-0.1.0.0"
sysconfdir = "/app/.stack-work/install/x86_64-linux-tinfo6/dedd53dcbe9a53bc94c4f9e1e6597e15d4d7393d570b4a26f9e6bcdce0b91d81/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "HAL_a2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "HAL_a2_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "HAL_a2_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "HAL_a2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "HAL_a2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "HAL_a2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

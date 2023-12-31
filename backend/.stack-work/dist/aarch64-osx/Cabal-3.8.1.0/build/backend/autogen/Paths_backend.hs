{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_backend (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/dannymckinney/Real-Time-Weather-Analysis-App/backend/.stack-work/install/aarch64-osx/271c152fe881f23c1e62408ec3935d709b219894529d86504cf3e8810b7267d2/9.4.7/bin"
libdir     = "/Users/dannymckinney/Real-Time-Weather-Analysis-App/backend/.stack-work/install/aarch64-osx/271c152fe881f23c1e62408ec3935d709b219894529d86504cf3e8810b7267d2/9.4.7/lib/aarch64-osx-ghc-9.4.7/backend-0.1.0.0-DFjSjthr9Ty2gHB0LrrsHN-backend"
dynlibdir  = "/Users/dannymckinney/Real-Time-Weather-Analysis-App/backend/.stack-work/install/aarch64-osx/271c152fe881f23c1e62408ec3935d709b219894529d86504cf3e8810b7267d2/9.4.7/lib/aarch64-osx-ghc-9.4.7"
datadir    = "/Users/dannymckinney/Real-Time-Weather-Analysis-App/backend/.stack-work/install/aarch64-osx/271c152fe881f23c1e62408ec3935d709b219894529d86504cf3e8810b7267d2/9.4.7/share/aarch64-osx-ghc-9.4.7/backend-0.1.0.0"
libexecdir = "/Users/dannymckinney/Real-Time-Weather-Analysis-App/backend/.stack-work/install/aarch64-osx/271c152fe881f23c1e62408ec3935d709b219894529d86504cf3e8810b7267d2/9.4.7/libexec/aarch64-osx-ghc-9.4.7/backend-0.1.0.0"
sysconfdir = "/Users/dannymckinney/Real-Time-Weather-Analysis-App/backend/.stack-work/install/aarch64-osx/271c152fe881f23c1e62408ec3935d709b219894529d86504cf3e8810b7267d2/9.4.7/etc"

getBinDir     = catchIO (getEnv "backend_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "backend_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "backend_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "backend_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "backend_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "backend_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'

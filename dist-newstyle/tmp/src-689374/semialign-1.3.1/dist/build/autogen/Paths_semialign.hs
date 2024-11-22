{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_semialign (
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
version = Version [1,3,1] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/var/home/jake/.local/state/cabal/store/ghc-9.4.8/semialign-1.3.1-a2bc3f78018e7d3f6440b2ebcebf5881049a5dc9717c226766e8a9dee0d5890f/bin"
libdir     = "/var/home/jake/.local/state/cabal/store/ghc-9.4.8/semialign-1.3.1-a2bc3f78018e7d3f6440b2ebcebf5881049a5dc9717c226766e8a9dee0d5890f/lib"
dynlibdir  = "/var/home/jake/.local/state/cabal/store/ghc-9.4.8/semialign-1.3.1-a2bc3f78018e7d3f6440b2ebcebf5881049a5dc9717c226766e8a9dee0d5890f/lib"
datadir    = "/var/home/jake/.local/state/cabal/store/ghc-9.4.8/semialign-1.3.1-a2bc3f78018e7d3f6440b2ebcebf5881049a5dc9717c226766e8a9dee0d5890f/share"
libexecdir = "/var/home/jake/.local/state/cabal/store/ghc-9.4.8/semialign-1.3.1-a2bc3f78018e7d3f6440b2ebcebf5881049a5dc9717c226766e8a9dee0d5890f/libexec"
sysconfdir = "/var/home/jake/.local/state/cabal/store/ghc-9.4.8/semialign-1.3.1-a2bc3f78018e7d3f6440b2ebcebf5881049a5dc9717c226766e8a9dee0d5890f/etc"

getBinDir     = catchIO (getEnv "semialign_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "semialign_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "semialign_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "semialign_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "semialign_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "semialign_sysconfdir") (\_ -> return sysconfdir)



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

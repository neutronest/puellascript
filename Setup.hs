{-# OPTIONS_GHC -Wall #-}

module Main where

import Data.Binary
import Data.Char
import Data.Maybe
import Distribution.Simple
import Distribution.Simple.InstallDirs
import Distribution.Simple.Program
import Distribution.Simple.Setup
import Distribution.System
import Distribution.Types.BuildInfo
import Distribution.Types.LocalBuildInfo
import System.FilePath

main :: IO ()
main =
    defaultMainWithHooks
        simpleUserHooks
        { postConf =
              \_args flags pkg_descr lbi -> do
                  writeFile "lbi.txt" $ show lbi
                  let ghc =
                          fromJust $ lookupProgram ghcProgram $ withPrograms lbi
                      ghcPkg =
                          fromJust $
                          lookupProgram ghcPkgProgram $ withPrograms lbi
                      phc =
                          (bindir $
                           absoluteInstallDirs
                               (localPackage lbi)
                               (localUnitId lbi)
                               (compilerInfo $ compiler lbi)
                               NoCopyDest
                               (hostPlatform lbi) $
                           installDirTemplates lbi) </>
                          "phc" <.>
                          (case hostPlatform lbi of
                               Platform _ Windows -> "exe"
                               _ -> "")
                      SpecificPackageDB pkgdb = last $ withPackageDB lbi
                  libdir' <-
                      getProgramOutput
                          (fromFlag (configVerbosity flags))
                          ghc
                          ["--print-libdir"]
                  encodeFile biPath $
                      emptyBuildInfo
                      { cppOptions =
                            [ "-DGHC=" ++ show (programPath ghc)
                            , "-DGHC_PKG=" ++ show (programPath ghcPkg)
                            , "-DLIBDIR=" ++
                              show (takeWhile (not . isSpace) libdir')
                            , "-DPKGDB=" ++ show pkgdb
                            , "-DPHC=" ++ show phc
                            ]
                      }
                  postConf simpleUserHooks _args flags pkg_descr lbi
        , preBuild = preAny
        , preRepl = preAny
        , preHaddock = preAny
        }
  where
    biPath = "puellascript.buildinfo"
    preAny _ _ = do
        bi <- decodeFile biPath
        pure (Just bi, [])

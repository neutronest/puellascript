{-# OPTIONS_GHC -Wall #-}

module Main where

import Data.Binary
import Data.Char
import Data.Maybe
import Distribution.Simple
import Distribution.Simple.Program
import Distribution.Simple.Setup
import Distribution.Types.BuildInfo
import Distribution.Types.LocalBuildInfo

main :: IO ()
main =
    defaultMainWithHooks
        simpleUserHooks
        { postConf =
              \_args flags pkg_descr lbi -> do
                  let ghc =
                          fromJust $ lookupProgram ghcProgram $ withPrograms lbi
                      ghcPkg =
                          fromJust $
                          lookupProgram ghcPkgProgram $ withPrograms lbi
                      SpecificPackageDB pkgdb = last $ withPackageDB lbi
                  libdir' <-
                      getProgramOutput
                          (fromFlag (configVerbosity flags))
                          ghc
                          ["--print-libdir"]
                  let libdir = takeWhile (\c -> not $ isSpace c) libdir'
                  encodeFile biPath $
                      emptyBuildInfo
                      { cppOptions =
                            [ "-DGHC=" ++ show (programPath ghc)
                            , "-DGHC_PKG=" ++ show (programPath ghcPkg)
                            , "-DLIBDIR=" ++ show libdir
                            , "-DPKGDB=" ++ show pkgdb
                            ]
                      }
                  postConf simpleUserHooks _args flags pkg_descr lbi
        , preBuild = preAny
        , preRepl = preAny
        }
  where
    biPath = "puellascript.buildinfo"
    preAny _ _ = do
        bi <- decodeFile biPath
        pure (Just bi, [])

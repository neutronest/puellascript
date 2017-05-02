{-# OPTIONS_GHC -Wall #-}

module Main where

import Data.Binary
import Data.Char
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
                  case lookupProgram ghcProgram $ withPrograms lbi of
                      Just ghc -> do
                          libdir' <-
                              getProgramOutput
                                  (fromFlag (configVerbosity flags))
                                  ghc
                                  ["--print-libdir"]
                          let libdir = takeWhile (\c -> not $ isSpace c) libdir'
                          case lookupProgram ghcPkgProgram $ withPrograms lbi of
                              Just ghcPkg -> do
                                  let SpecificPackageDB pkgdb =
                                          last $ withPackageDB lbi
                                  encodeFile biPath $
                                      emptyBuildInfo
                                      { cppOptions =
                                            [ "-DGHC=" ++ show (programPath ghc)
                                            , "-DGHC_PKG=" ++
                                              show (programPath ghcPkg)
                                            , "-DLIBDIR=" ++ show libdir
                                            , "-DPKGDB=" ++ show pkgdb
                                            ]
                                      }
                                  postConf
                                      simpleUserHooks
                                      _args
                                      flags
                                      pkg_descr
                                      lbi
                              Nothing -> fail "ghc-pkg not found."
                      Nothing -> fail "ghc not found."
        , preBuild = preAny
        , preRepl = preAny
        }
  where
    biPath = "puellascript.buildinfo"
    preAny _ _ = do
        bi <- decodeFile biPath
        pure (Just bi, [])

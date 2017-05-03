module Main where

import Language.PuellaScript.Compiler.SelfBuildInfo
import System.Environment
import System.Process

main :: IO ()
main = do
    args <- getArgs
    callProcess
        ghc
        [ arg
        | arg' <- args
        , arg <-
              if arg' == "--make"
                  then [ "--frontend"
                       , "Language.PuellaScript.Compiler.FrontendPlugin"
                       , "-plugin-package"
                       , "puellascript"
                       , "-package-db"
                       , pkgDb
                       ]
                  else [arg']
        ]

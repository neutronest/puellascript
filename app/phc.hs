module Main where

import Language.PuellaScript.Compiler.SelfBuildInfo
import System.Environment
import System.Process

main :: IO ()
main = do
    args <- getArgs
    callProcess ghc args

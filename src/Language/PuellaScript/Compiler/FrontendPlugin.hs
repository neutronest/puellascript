module Language.PuellaScript.Compiler.FrontendPlugin where

import qualified GhcPlugins as GHC

frontendPlugin :: GHC.FrontendPlugin
frontendPlugin = GHC.defaultFrontendPlugin

module Language.PuellaScript.Compiler.FrontendPlugin where

import Control.Monad.IO.Class
import qualified GhcPlugins as GHC

frontendPlugin :: GHC.FrontendPlugin
frontendPlugin =
    GHC.defaultFrontendPlugin
    { GHC.frontend =
          \args targets ->
              liftIO $
              putStrLn $ "args: " ++ show args ++ "\ntargets: " ++ show targets
    }

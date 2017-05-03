module Language.PuellaScript.Compiler.FrontendPlugin where

import Data.Functor
import qualified GHC
import qualified GhcPlugins as GHC

frontendPlugin :: GHC.FrontendPlugin
frontendPlugin =
    GHC.defaultFrontendPlugin
    { GHC.frontend =
          \_ targets -> do
              dflags <- GHC.getSessionDynFlags
              void $
                  GHC.setSessionDynFlags dflags {GHC.ghcMode = GHC.CompManager}
              ts <- sequenceA [GHC.guessTarget t f | (t, f) <- targets]
              GHC.setTargets ts
              sf <- GHC.load GHC.LoadAllTargets
              case sf of
                  GHC.Succeeded -> pure ()
                  GHC.Failed -> fail "GHC.load failed."
    }

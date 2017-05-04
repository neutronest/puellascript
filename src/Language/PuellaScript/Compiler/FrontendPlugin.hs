module Language.PuellaScript.Compiler.FrontendPlugin where

import Control.Monad.IO.Class
import Data.Functor
import qualified DriverPipeline as GHC
import qualified GHC
import qualified GhcPlugins as GHC
import qualified Hooks as GHC

runPhaseHook
    :: GHC.PhasePlus
    -> FilePath
    -> GHC.DynFlags
    -> GHC.CompPipeline (GHC.PhasePlus, FilePath)
runPhaseHook phase_plus input_fn dflags = do
    liftIO $
        putStrLn $
        "runPhase " ++ GHC.showPpr dflags phase_plus ++ " " ++ input_fn
    GHC.runPhase phase_plus input_fn dflags

frontendPlugin :: GHC.FrontendPlugin
frontendPlugin =
    GHC.defaultFrontendPlugin
    { GHC.frontend =
          \_ targets -> do
              dflags <- GHC.getSessionDynFlags
              void $
                  GHC.setSessionDynFlags
                      dflags
                      { GHC.ghcMode = GHC.CompManager
                      , GHC.hooks =
                            GHC.emptyHooks
                            {GHC.runPhaseHook = Just runPhaseHook}
                      }
              ts <- sequenceA [GHC.guessTarget t f | (t, f) <- targets]
              GHC.setTargets ts
              sf <- GHC.load GHC.LoadAllTargets
              case sf of
                  GHC.Succeeded -> pure ()
                  GHC.Failed -> fail "GHC.load failed."
    }

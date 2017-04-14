{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE Strict #-}

module Language.PuellaScript.Compiler.Run where

import Control.Monad
import Control.Monad.IO.Class
import qualified DriverPipeline as GHC
import qualified DynFlags as GHC
import qualified GHC
import qualified GHC.Paths as GHC
import qualified Hooks as GHC
import qualified HscTypes as GHC
import Language.PuellaScript.Compiler.Flags

run :: Flags -> IO ()
run flags =
    GHC.defaultErrorHandler (fatalMessager flags) GHC.defaultFlushOut $
    GHC.runGhc (Just GHC.libdir) $ do
        dflags <- GHC.getSessionDynFlags
        void $
            GHC.setSessionDynFlags
                dflags
                { GHC.verbosity = 3
                , GHC.optLevel = 2
                , GHC.importPaths = importPaths flags ++ GHC.importPaths dflags
                , GHC.hooks =
                      GHC.emptyHooks {GHC.runPhaseHook = Just run_phase_hook}
                , GHC.language = Just GHC.Haskell2010
                }
        sequence [GHC.guessTarget t Nothing | t <- targets flags] >>=
            GHC.setTargets
        sflag <- GHC.load GHC.LoadAllTargets
        when (GHC.failed sflag) $ fail "GHC.load returned Failed."
  where
    run_phase_hook
        :: GHC.PhasePlus
        -> FilePath
        -> GHC.DynFlags
        -> GHC.CompPipeline (GHC.PhasePlus, FilePath)
    run_phase_hook phase_plus input_fn dflags = do
        result <- GHC.runPhase phase_plus input_fn dflags
        liftIO $
            case phase_plus of
                GHC.HscOut _ _ (GHC.HscRecomp cg_guts mod_summary) ->
                    coreHook flags cg_guts mod_summary
                _ -> pure ()
        pure result

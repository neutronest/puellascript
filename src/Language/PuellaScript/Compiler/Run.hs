{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

module Language.PuellaScript.Compiler.Run where

import qualified CmmBuildInfoTables as GHC
import qualified CmmInfo as GHC
import qualified CmmPipeline as GHC
import Control.Monad
import Control.Monad.IO.Class
import qualified CorePrep as GHC
import qualified CoreToStg as GHC
import qualified DriverPhases as GHC
import qualified DriverPipeline as GHC
import qualified DynFlags as GHC
import qualified GHC
import qualified Hooks as GHC
import qualified HscMain as GHC
import qualified HscTypes as GHC
import Language.PuellaScript.Compiler.Flags
import Language.PuellaScript.Compiler.SelfBuildInfo
import qualified SimplStg as GHC
import qualified StgCmm
import qualified Stream
import qualified TyCon as GHC
import qualified UniqSupply as GHC

run :: Flags -> IO ()
run flags =
    GHC.defaultErrorHandler (fatalMessager flags) GHC.defaultFlushOut $
    GHC.runGhc (Just libDir) $ do
        dflags <- GHC.getSessionDynFlags
        void $
            GHC.setSessionDynFlags $
            dflags
            { GHC.ghcLink = GHC.NoLink
            , GHC.hscTarget = GHC.HscAsm
            , GHC.verbosity = 3
            , GHC.optLevel = 2
            , GHC.importPaths = importPaths flags
            , GHC.packageDBFlags = packageDBFlags flags
            , GHC.hooks =
                  GHC.emptyHooks {GHC.runPhaseHook = Just run_phase_hook}
            , GHC.log_action = logAction flags
            } `GHC.lang_set`
            Just GHC.Haskell2010 `GHC.gopt_set`
            GHC.Opt_SplitSections
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
    run_phase_hook phase_plus input_fn dflags =
        case phase_plus of
            GHC.HscOut _ _ (GHC.HscRecomp cg_guts@GHC.CgGuts {..} mod_summary) -> do
                liftIO $ coreHook flags mod_summary cg_guts
                GHC.PipeState {..} <- GHC.getPipeState
                liftIO $ do
                    let data_tycons = filter GHC.isDataTyCon cg_tycons
                        location = GHC.ms_location mod_summary
                    prepd_binds <-
                        GHC.corePrepPgm
                            hsc_env
                            cg_module
                            location
                            cg_binds
                            data_tycons
                    corePrepHook flags mod_summary prepd_binds
                    let stg_binds_from_core =
                            GHC.coreToStg dflags cg_module prepd_binds
                    stgFromCoreHook flags mod_summary stg_binds_from_core
                    (stg_binds, cost_centre_info) <-
                        GHC.stg2stg dflags cg_module stg_binds_from_core
                    stgHook flags mod_summary stg_binds
                    us <- GHC.mkSplitUniqSupply 'S'
                    let cmm_stream =
                            StgCmm.codeGen
                                dflags
                                cg_module
                                data_tycons
                                cost_centre_info
                                stg_binds
                                cg_hpc_info
                        cmms =
                            let run_pipeline us' cmmgroup = do
                                    let (topSRT', us'') =
                                            GHC.initUs us' GHC.emptySRT
                                    (topSRT, cmmgroup') <-
                                        GHC.cmmPipeline hsc_env topSRT' cmmgroup
                                    let srt
                                            | GHC.isEmptySRT topSRT = []
                                            | otherwise = GHC.srtToData topSRT
                                    pure (us'', srt ++ cmmgroup')
                            in void $
                               Stream.mapAccumL run_pipeline us cmm_stream
                    cmmFromStgHook flags mod_summary cmm_stream
                    cmmHook flags mod_summary cmms
                    rawcmms <- GHC.cmmToRawCmm dflags cmms
                    cmmRawHook flags mod_summary rawcmms
                output_fn <- GHC.phaseOutputFilename GHC.StopLn
                liftIO $ writeFile output_fn ""
                pure (GHC.RealPhase GHC.StopLn, output_fn)
            _ -> GHC.runPhase phase_plus input_fn dflags

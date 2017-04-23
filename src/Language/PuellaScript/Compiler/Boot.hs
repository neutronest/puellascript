{-# LANGUAGE RecordWildCards #-}

module Language.PuellaScript.Compiler.Boot where

import Control.Monad.IO.Class
import Data.Functor
import Data.IORef
import qualified DynFlags as GHC
import qualified GHC
import qualified GHC.Paths as GHC
import qualified HscTypes as GHC
import qualified Module as GHC
import qualified Outputable as GHC

boot :: IO ()
boot =
    GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $
    GHC.runGhc (Just GHC.libdir) $ do
        dflags <- GHC.getSessionDynFlags
        void $
            GHC.setSessionDynFlags $
            dflags
            { GHC.ghcLink = GHC.NoLink
            , GHC.hscTarget = GHC.HscAsm
            , GHC.verbosity = 3
            , GHC.optLevel = 2
            } `GHC.lang_set`
            Just GHC.Haskell2010 `GHC.gopt_set`
            GHC.Opt_SplitSections
        GHC.HscEnv {..} <- GHC.getSession
        liftIO $ do
            GHC.EPS {..} <- readIORef hsc_EPS
            putStrLn $ GHC.showPpr dflags $ GHC.moduleEnvKeys eps_PIT

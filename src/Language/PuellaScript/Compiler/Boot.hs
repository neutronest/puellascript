module Language.PuellaScript.Compiler.Boot where

import Data.Functor
import qualified DynFlags as GHC
import qualified GHC
import qualified GHC.Paths as GHC

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
        pure ()

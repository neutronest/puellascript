{-# LANGUAGE StrictData #-}

module Language.PuellaScript.Compiler.Flags where

import qualified Cmm as GHC
import qualified CoreSyn as GHC
import qualified DynFlags as GHC
import qualified HscTypes as GHC
import qualified StgSyn as GHC
import qualified Stream as GHC

data Flags = Flags
    { importPaths :: [FilePath]
    , targets :: [String]
    , packageDBFlags :: [GHC.PackageDBFlag]
    , fatalMessager :: GHC.FatalMessager
    , logAction :: GHC.LogAction
    , coreHook :: GHC.ModSummary -> GHC.CgGuts -> IO ()
    , corePrepHook :: GHC.ModSummary -> GHC.CoreProgram -> IO ()
    , stgFromCoreHook :: GHC.ModSummary -> [GHC.StgTopBinding] -> IO ()
    , stgHook :: GHC.ModSummary -> [GHC.StgTopBinding] -> IO ()
    , cmmFromStgHook :: GHC.ModSummary -> GHC.Stream IO GHC.CmmGroup () -> IO ()
    , cmmHook :: GHC.ModSummary -> GHC.Stream IO GHC.CmmGroup () -> IO ()
    , cmmRawHook :: GHC.ModSummary -> GHC.Stream IO GHC.RawCmmGroup () -> IO ()
    }

defaultFlags :: Flags
defaultFlags =
    Flags
    { importPaths = []
    , targets = []
    , packageDBFlags = []
    , fatalMessager = GHC.defaultFatalMessager
    , logAction = GHC.defaultLogAction
    , coreHook = \_ _ -> pure ()
    , corePrepHook = \_ _ -> pure ()
    , stgFromCoreHook = \_ _ -> pure ()
    , stgHook = \_ _ -> pure ()
    , cmmFromStgHook = \_ _ -> pure ()
    , cmmHook = \_ _ -> pure ()
    , cmmRawHook = \_ _ -> pure ()
    }

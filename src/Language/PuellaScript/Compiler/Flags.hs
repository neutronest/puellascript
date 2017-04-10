{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

module Language.PuellaScript.Compiler.Flags where

import Data.Functor
import qualified DynFlags as GHC
import qualified GHC
import qualified GHC.Paths as GHC

data Flag = Flag
    { importPaths :: [FilePath]
    , targets :: [String]
    , fatalMessager :: GHC.FatalMessager
    , flushOut :: GHC.FlushOut
    , logFinaliser :: GHC.LogFinaliser
    , libDir :: Maybe FilePath
    }

defaultFlag :: Flag
defaultFlag =
    Flag
    { importPaths = []
    , targets = []
    , fatalMessager = GHC.defaultFatalMessager
    , flushOut = GHC.defaultFlushOut
    , logFinaliser = const $ pure ()
    , libDir = Just GHC.libdir
    }

applyFlag
    :: GHC.GhcMonad m
    => Flag -> m ()
applyFlag Flag {..} = do
    dflags <- GHC.getProgramDynFlags
    void $
        GHC.setProgramDynFlags
            dflags
            { GHC.importPaths = importPaths ++ GHC.importPaths dflags
            , GHC.log_finaliser = logFinaliser
            }
    traverse (`GHC.guessTarget` Nothing) targets >>= GHC.setTargets

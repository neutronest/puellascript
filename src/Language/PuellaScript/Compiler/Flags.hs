{-# LANGUAGE StrictData #-}

module Language.PuellaScript.Compiler.Flags where

import qualified DynFlags as GHC
import qualified HscTypes as GHC

data Flags = Flags
    { importPaths :: [FilePath]
    , targets :: [String]
    , fatalMessager :: GHC.FatalMessager
    , coreHook :: GHC.CgGuts -> GHC.ModSummary -> IO ()
    }

defaultFlags :: Flags
defaultFlags =
    Flags
    { importPaths = []
    , targets = []
    , fatalMessager = GHC.defaultFatalMessager
    , coreHook = \_ _ -> pure ()
    }

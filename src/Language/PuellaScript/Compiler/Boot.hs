{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

module Language.PuellaScript.Compiler.Boot where

import Language.PuellaScript.Compiler.SelfBuildInfo
import System.Directory
import System.Exit
import System.FilePath
import System.Process

data BootConfig = BootConfig
    { bootDir, bootPkgDb, bootLibDir :: FilePath
    }

defaultBootConfig :: IO BootConfig
defaultBootConfig = do
    wd <- getCurrentDirectory
    pure
        BootConfig
        { bootDir = wd </> ".boot"
        , bootPkgDb = wd </> ".boot" </> "package.conf.d"
        , bootLibDir = wd </> "boot-libraries"
        }

callIn :: FilePath -> FilePath -> [String] -> IO ()
callIn wd proc' args =
    withCreateProcess ((proc proc' args) {cwd = Just wd}) $ \_ _ _ h -> do
        r <- waitForProcess h
        case r of
            ExitSuccess -> pure ()
            ExitFailure x ->
                fail $
                "Running " ++
                show proc' ++
                " " ++
                show args ++ " in " ++ wd ++ " failed with exit code " ++ show x

callGhcPkg :: BootConfig -> [String] -> IO ()
callGhcPkg BootConfig {..} args =
    callProcess ghcPkg $ ("--package-db=" ++ bootPkgDb) : args

nukeBootDir :: BootConfig -> IO ()
nukeBootDir BootConfig {..} = callProcess "rm" ["-rf", bootDir]

initBootDir :: BootConfig -> IO ()
initBootDir bc@BootConfig {..} = do
    callProcess "mkdir" ["-p", bootPkgDb]
    callGhcPkg bc ["recache"]

installRts :: BootConfig -> IO ()
installRts bc@BootConfig {..} = do
    callProcess
        "cp"
        [libDir </> "package.conf.d" </> "rts.conf", bootPkgDb </> "rts.conf"]
    callGhcPkg bc ["recache"]

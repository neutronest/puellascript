{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}

module GHC.Environment (getFullArgs) where

import Foreign
import Foreign.C
import GHC.Base
import GHC.Real ( fromIntegral )

#ifdef mingw32_HOST_OS
import GHC.IO (finally)
import GHC.Windows

# if defined(i386_HOST_ARCH)
#  define WINDOWS_CCONV stdcall
# elif defined(x86_64_HOST_ARCH)
#  define WINDOWS_CCONV ccall
# else
#  error Unknown mingw32 arch
# endif
#else
import GHC.IO.Encoding
import qualified GHC.Foreign as GHC
#endif

-- | Computation 'getFullArgs' is the "raw" version of 'getArgs', similar
-- to @argv@ in other languages. It returns a list of the program's
-- command line arguments, starting with the program name, and
-- including those normally eaten by the RTS (+RTS ... -RTS).
getFullArgs :: IO [String]
#ifdef mingw32_HOST_OS
-- Ignore the arguments to hs_init on Windows for the sake of Unicode compat
getFullArgs = do
    p_arg_string <- c_GetCommandLine
    alloca $ \p_argc -> do
     p_argv <- c_CommandLineToArgv p_arg_string p_argc
     if p_argv == nullPtr
      then throwGetLastError "getFullArgs"
      else flip finally (c_LocalFree p_argv) $ do
       argc <- peek p_argc
       p_argvs <- peekArray (fromIntegral argc) p_argv
       mapM peekCWString p_argvs

c_GetCommandLine :: IO (Ptr CWString)
c_GetCommandLine = c_GetCommandLine

c_CommandLineToArgv :: Ptr CWString -> Ptr CInt -> IO (Ptr CWString)
c_CommandLineToArgv = c_CommandLineToArgv

c_LocalFree :: Ptr a -> IO (Ptr a)
c_LocalFree = c_LocalFree
#else
getFullArgs =
  alloca $ \ p_argc ->
  alloca $ \ p_argv -> do
   getFullProgArgv p_argc p_argv
   p    <- fromIntegral `liftM` peek p_argc
   argv <- peek p_argv
   enc <- getFileSystemEncoding
   peekArray p argv >>= mapM (GHC.peekCString enc)

getFullProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()
getFullProgArgv = getFullProgArgv
#endif

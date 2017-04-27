{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Internals
-- Copyright   :  (c) The University of Glasgow, 1992-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (requires POSIX)
--
-- POSIX support layer for the standard libraries.
-- This library is built on *every* platform, including Win32.
--
-- Non-posix compliant in order to support the following features:
--      * S_ISSOCK (no sockets in POSIX)
--
-----------------------------------------------------------------------------

module System.Posix.Internals where

#include "HsBaseConfig.h"

import System.Posix.Types

import Foreign
import Foreign.C

-- import Data.Bits
import Data.Maybe

#if !defined(HTYPE_TCFLAG_T)
import System.IO.Error
#endif

import GHC.Base
import GHC.Num
import GHC.Real
import GHC.IO
import GHC.IO.IOMode
import GHC.IO.Exception
import GHC.IO.Device
#ifndef mingw32_HOST_OS
import {-# SOURCE #-} GHC.IO.Encoding (getFileSystemEncoding)
import qualified GHC.Foreign as GHC
#endif

-- ---------------------------------------------------------------------------
-- Debugging the base package

puts :: String -> IO ()
puts s = withCAStringLen (s ++ "\n") $ \(p, len) -> do
            -- In reality should be withCString, but assume ASCII to avoid loop
            -- if this is called by GHC.Foreign
           _ <- c_write 1 (castPtr p) (fromIntegral len)
           return ()


-- ---------------------------------------------------------------------------
-- Types

data {-# CTYPE "struct flock" #-} CFLock
data {-# CTYPE "struct group" #-} CGroup
data {-# CTYPE "struct lconv" #-} CLconv
data {-# CTYPE "struct passwd" #-} CPasswd
data {-# CTYPE "struct sigaction" #-} CSigaction
data {-# CTYPE "sigset_t" #-} CSigset
data {-# CTYPE "struct stat" #-}  CStat
data {-# CTYPE "struct termios" #-} CTermios
data {-# CTYPE "struct tm" #-} CTm
data {-# CTYPE "struct tms" #-} CTms
data {-# CTYPE "struct utimbuf" #-} CUtimbuf
data {-# CTYPE "struct utsname" #-} CUtsname

type FD = CInt

-- ---------------------------------------------------------------------------
-- stat()-related stuff

fdFileSize :: FD -> IO Integer
fdFileSize fd =
  allocaBytes sizeof_stat $ \ p_stat -> do
    throwErrnoIfMinus1Retry_ "fileSize" $
        c_fstat fd p_stat
    c_mode <- st_mode p_stat :: IO CMode
    if not (s_isreg c_mode)
        then return (-1)
        else do
      c_size <- st_size p_stat
      return (fromIntegral c_size)

fileType :: FilePath -> IO IODeviceType
fileType file =
  allocaBytes sizeof_stat $ \ p_stat -> do
  withFilePath file $ \p_file -> do
    throwErrnoIfMinus1Retry_ "fileType" $
      c_stat p_file p_stat
    statGetType p_stat

-- NOTE: On Win32 platforms, this will only work with file descriptors
-- referring to file handles. i.e., it'll fail for socket FDs.
fdStat :: FD -> IO (IODeviceType, CDev, CIno)
fdStat fd =
  allocaBytes sizeof_stat $ \ p_stat -> do
    throwErrnoIfMinus1Retry_ "fdType" $
        c_fstat fd p_stat
    ty <- statGetType p_stat
    dev <- st_dev p_stat
    ino <- st_ino p_stat
    return (ty,dev,ino)

fdType :: FD -> IO IODeviceType
fdType fd = do (ty,_,_) <- fdStat fd; return ty

statGetType :: Ptr CStat -> IO IODeviceType
statGetType p_stat = do
  c_mode <- st_mode p_stat :: IO CMode
  case () of
      _ | s_isdir c_mode        -> return Directory
        | s_isfifo c_mode || s_issock c_mode || s_ischr  c_mode
                                -> return Stream
        | s_isreg c_mode        -> return RegularFile
         -- Q: map char devices to RawDevice too?
        | s_isblk c_mode        -> return RawDevice
        | otherwise             -> ioError ioe_unknownfiletype

ioe_unknownfiletype :: IOException
ioe_unknownfiletype = IOError Nothing UnsupportedOperation "fdType"
                        "unknown file type"
                        Nothing
                        Nothing

fdGetMode :: FD -> IO IOMode
#if defined(mingw32_HOST_OS)
fdGetMode _ = do
    -- We don't have a way of finding out which flags are set on FDs
    -- on Windows, so make a handle that thinks that anything goes.
    let flags = o_RDWR
#else
fdGetMode fd = do
    flags <- throwErrnoIfMinus1Retry "fdGetMode"
                (c_fcntl_read fd const_f_getfl)
#endif
    let
       wH  = (flags .&. o_WRONLY) /= 0
       aH  = (flags .&. o_APPEND) /= 0
       rwH = (flags .&. o_RDWR) /= 0

       mode
         | wH && aH  = AppendMode
         | wH        = WriteMode
         | rwH       = ReadWriteMode
         | otherwise = ReadMode

    return mode

#ifdef mingw32_HOST_OS
withFilePath :: FilePath -> (CWString -> IO a) -> IO a
withFilePath = withCWString

newFilePath :: FilePath -> IO CWString
newFilePath = newCWString

peekFilePath :: CWString -> IO FilePath
peekFilePath = peekCWString
#else

withFilePath :: FilePath -> (CString -> IO a) -> IO a
newFilePath :: FilePath -> IO CString
peekFilePath :: CString -> IO FilePath
peekFilePathLen :: CStringLen -> IO FilePath

withFilePath fp f = getFileSystemEncoding >>= \enc -> GHC.withCString enc fp f
newFilePath fp = getFileSystemEncoding >>= \enc -> GHC.newCString enc fp
peekFilePath fp = getFileSystemEncoding >>= \enc -> GHC.peekCString enc fp
peekFilePathLen fp = getFileSystemEncoding >>= \enc -> GHC.peekCStringLen enc fp

#endif

-- ---------------------------------------------------------------------------
-- Terminal-related stuff

#if defined(HTYPE_TCFLAG_T)

setEcho :: FD -> Bool -> IO ()
setEcho fd on = do
  tcSetAttr fd $ \ p_tios -> do
    lflag <- c_lflag p_tios :: IO CTcflag
    let new_lflag
         | on        = lflag .|. fromIntegral const_echo
         | otherwise = lflag .&. complement (fromIntegral const_echo)
    poke_c_lflag p_tios (new_lflag :: CTcflag)

getEcho :: FD -> IO Bool
getEcho fd = do
  tcSetAttr fd $ \ p_tios -> do
    lflag <- c_lflag p_tios :: IO CTcflag
    return ((lflag .&. fromIntegral const_echo) /= 0)

setCooked :: FD -> Bool -> IO ()
setCooked fd cooked =
  tcSetAttr fd $ \ p_tios -> do

    -- turn on/off ICANON
    lflag <- c_lflag p_tios :: IO CTcflag
    let new_lflag | cooked    = lflag .|. (fromIntegral const_icanon)
                  | otherwise = lflag .&. complement (fromIntegral const_icanon)
    poke_c_lflag p_tios (new_lflag :: CTcflag)

    -- set VMIN & VTIME to 1/0 respectively
    when (not cooked) $ do
            c_cc <- ptr_c_cc p_tios
            let vmin  = (c_cc `plusPtr` (fromIntegral const_vmin))  :: Ptr Word8
                vtime = (c_cc `plusPtr` (fromIntegral const_vtime)) :: Ptr Word8
            poke vmin  1
            poke vtime 0

tcSetAttr :: FD -> (Ptr CTermios -> IO a) -> IO a
tcSetAttr fd fun = do
     allocaBytes sizeof_termios  $ \p_tios -> do
        throwErrnoIfMinus1Retry_ "tcSetAttr"
           (c_tcgetattr fd p_tios)

        -- Save a copy of termios, if this is a standard file descriptor.
        -- These terminal settings are restored in hs_exit().
        when (fd <= 2) $ do
          p <- get_saved_termios fd
          when (p == nullPtr) $ do
             saved_tios <- mallocBytes sizeof_termios
             copyBytes saved_tios p_tios sizeof_termios
             set_saved_termios fd saved_tios

        -- tcsetattr() when invoked by a background process causes the process
        -- to be sent SIGTTOU regardless of whether the process has TOSTOP set
        -- in its terminal flags (try it...).  This function provides a
        -- wrapper which temporarily blocks SIGTTOU around the call, making it
        -- transparent.
        allocaBytes sizeof_sigset_t $ \ p_sigset -> do
          allocaBytes sizeof_sigset_t $ \ p_old_sigset -> do
             throwErrnoIfMinus1_ "sigemptyset" $
                 c_sigemptyset p_sigset
             throwErrnoIfMinus1_ "sigaddset" $
                 c_sigaddset   p_sigset const_sigttou
             throwErrnoIfMinus1_ "sigprocmask" $
                 c_sigprocmask const_sig_block p_sigset p_old_sigset
             r <- fun p_tios  -- do the business
             throwErrnoIfMinus1Retry_ "tcSetAttr" $
                 c_tcsetattr fd const_tcsanow p_tios
             throwErrnoIfMinus1_ "sigprocmask" $
                 c_sigprocmask const_sig_setmask p_old_sigset nullPtr
             return r

get_saved_termios :: CInt -> IO (Ptr CTermios)
get_saved_termios = get_saved_termios

set_saved_termios :: CInt -> (Ptr CTermios) -> IO ()
set_saved_termios = set_saved_termios

#else

-- 'raw' mode for Win32 means turn off 'line input' (=> buffering and
-- character translation for the console.) The Win32 API for doing
-- this is GetConsoleMode(), which also requires echoing to be disabled
-- when turning off 'line input' processing. Notice that turning off
-- 'line input' implies enter/return is reported as '\r' (and it won't
-- report that character until another character is input..odd.) This
-- latter feature doesn't sit too well with IO actions like IO.hGetLine..
-- consider yourself warned.
setCooked :: FD -> Bool -> IO ()
setCooked fd cooked = do
  x <- set_console_buffering fd (if cooked then 1 else 0)
  if (x /= 0)
   then ioError (ioe_unk_error "setCooked" "failed to set buffering")
   else return ()

ioe_unk_error :: String -> String -> IOException
ioe_unk_error loc msg
 = ioeSetErrorString (mkIOError OtherError loc Nothing Nothing) msg

-- Note: echoing goes hand in hand with enabling 'line input' / raw-ness
-- for Win32 consoles, hence setEcho ends up being the inverse of setCooked.
setEcho :: FD -> Bool -> IO ()
setEcho fd on = do
  x <- set_console_echo fd (if on then 1 else 0)
  if (x /= 0)
   then ioError (ioe_unk_error "setEcho" "failed to set echoing")
   else return ()

getEcho :: FD -> IO Bool
getEcho fd = do
  r <- get_console_echo fd
  if (r == (-1))
   then ioError (ioe_unk_error "getEcho" "failed to get echoing")
   else return (r == 1)

set_console_buffering :: CInt -> CInt -> IO CInt
set_console_buffering = set_console_buffering

set_console_echo :: CInt -> CInt -> IO CInt
set_console_echo = set_console_echo

get_console_echo :: CInt -> IO CInt
get_console_echo = get_console_echo

is_console :: CInt -> IO CInt
is_console = is_console

#endif

-- ---------------------------------------------------------------------------
-- Turning on non-blocking for a file descriptor

setNonBlockingFD :: FD -> Bool -> IO ()
#if !defined(mingw32_HOST_OS)
setNonBlockingFD fd set = do
  flags <- throwErrnoIfMinus1Retry "setNonBlockingFD"
                 (c_fcntl_read fd const_f_getfl)
  let flags' | set       = flags .|. o_NONBLOCK
             | otherwise = flags .&. complement o_NONBLOCK
  when (flags /= flags') $ do
    -- An error when setting O_NONBLOCK isn't fatal: on some systems
    -- there are certain file handles on which this will fail (eg. /dev/null
    -- on FreeBSD) so we throw away the return code from fcntl_write.
    _ <- c_fcntl_write fd const_f_setfl (fromIntegral flags')
    return ()
#else

-- bogus defns for win32
setNonBlockingFD _ _ = return ()

#endif

-- -----------------------------------------------------------------------------
-- Set close-on-exec for a file descriptor

#if !defined(mingw32_HOST_OS)
setCloseOnExec :: FD -> IO ()
setCloseOnExec fd = do
  throwErrnoIfMinus1_ "setCloseOnExec" $
    c_fcntl_write fd const_f_setfd const_fd_cloexec
#endif

-- -----------------------------------------------------------------------------
-- foreign imports

#if !defined(mingw32_HOST_OS)
type CFilePath = CString
#else
type CFilePath = CWString
#endif

c_open :: CFilePath -> CInt -> CMode -> IO CInt
c_open = c_open

c_safe_open :: CFilePath -> CInt -> CMode -> IO CInt
c_safe_open = c_safe_open

c_fstat :: CInt -> Ptr CStat -> IO CInt
c_fstat = c_fstat

lstat :: CFilePath -> Ptr CStat -> IO CInt
lstat = lstat

{- Note: Win32 POSIX functions
Functions that are not part of the POSIX standards were
at some point deprecated by Microsoft. This deprecation
was performed by renaming the functions according to the
C++ ABI Section 17.6.4.3.2b. This was done to free up the
namespace of normal Windows programs since Windows isn't
POSIX compliant anyway.

These were working before since the RTS was re-exporting
these symbols under the undeprecated names. This is no longer
being done. See #11223

See https://msdn.microsoft.com/en-us/library/ms235384.aspx
for more.

However since we can't hope to get people to support Windows
packages we should support the deprecated names. See #12497
-}
c_lseek :: CInt -> COff -> CInt -> IO COff
c_lseek = c_lseek

c_access :: CString -> CInt -> IO CInt
c_access = c_access

c_chmod :: CString -> CMode -> IO CInt
c_chmod = c_chmod

c_close :: CInt -> IO CInt
c_close = c_close

c_creat :: CString -> CMode -> IO CInt
c_creat = c_creat

c_dup :: CInt -> IO CInt
c_dup = c_dup

c_dup2 :: CInt -> CInt -> IO CInt
c_dup2 = c_dup2

c_isatty :: CInt -> IO CInt
c_isatty = c_isatty

#if defined(mingw32_HOST_OS)
-- See Note: Windows types
c_read :: CInt -> Ptr Word8 -> CUInt -> IO CInt
c_read = c_read

-- See Note: Windows types
c_safe_read :: CInt -> Ptr Word8 -> CUInt -> IO CInt
c_safe_read = c_safe_read

c_umask :: CMode -> IO CMode
c_umask = c_umask

-- See Note: Windows types
c_write :: CInt -> Ptr Word8 -> CUInt -> IO CInt
c_write = c_write

-- See Note: Windows types
c_safe_write :: CInt -> Ptr Word8 -> CUInt -> IO CInt
c_safe_write = c_safe_write

c_pipe :: Ptr CInt -> IO CInt
c_pipe = c_pipe
#else
-- We use CAPI as on some OSs (eg. Linux) this is wrapped by a macro
-- which redirects to the 64-bit-off_t versions when large file
-- support is enabled.

-- See Note: Windows types
c_read :: CInt -> Ptr Word8 -> CSize -> IO CSsize
c_read = c_read

-- See Note: Windows types
c_safe_read :: CInt -> Ptr Word8 -> CSize -> IO CSsize
c_safe_read = c_safe_read

c_umask :: CMode -> IO CMode
c_umask = c_umask

-- See Note: Windows types
c_write :: CInt -> Ptr Word8 -> CSize -> IO CSsize
c_write = c_write

-- See Note: Windows types
c_safe_write :: CInt -> Ptr Word8 -> CSize -> IO CSsize
c_safe_write = c_safe_write

c_pipe :: Ptr CInt -> IO CInt
c_pipe = c_pipe
#endif

c_unlink :: CString -> IO CInt
c_unlink = c_unlink

c_utime :: CString -> Ptr CUtimbuf -> IO CInt
c_utime = c_utime

c_getpid :: IO CPid
c_getpid = c_getpid

c_stat :: CFilePath -> Ptr CStat -> IO CInt
c_stat = c_stat

c_ftruncate :: CInt -> COff -> IO CInt
c_ftruncate = c_ftruncate

#if !defined(mingw32_HOST_OS)
c_fcntl_read  :: CInt -> CInt -> IO CInt
c_fcntl_read = c_fcntl_read

c_fcntl_write :: CInt -> CInt -> CLong -> IO CInt
c_fcntl_write = c_fcntl_write

c_fcntl_lock  :: CInt -> CInt -> Ptr CFLock -> IO CInt
c_fcntl_lock = c_fcntl_lock

c_fork :: IO CPid
c_fork = c_fork

c_link :: CString -> CString -> IO CInt
c_link = c_link

-- capi is required at least on Android
c_mkfifo :: CString -> CMode -> IO CInt
c_mkfifo = c_mkfifo

c_sigemptyset :: Ptr CSigset -> IO CInt
c_sigemptyset = c_sigemptyset

c_sigaddset :: Ptr CSigset -> CInt -> IO CInt
c_sigaddset = c_sigaddset

c_sigprocmask :: CInt -> Ptr CSigset -> Ptr CSigset -> IO CInt
c_sigprocmask = c_sigprocmask

-- capi is required at least on Android
c_tcgetattr :: CInt -> Ptr CTermios -> IO CInt
c_tcgetattr = c_tcgetattr

-- capi is required at least on Android
c_tcsetattr :: CInt -> CInt -> Ptr CTermios -> IO CInt
c_tcsetattr = c_tcsetattr

c_waitpid :: CPid -> Ptr CInt -> CInt -> IO CPid
c_waitpid = c_waitpid
#endif

-- POSIX flags only:
o_RDONLY :: CInt
o_RDONLY = o_RDONLY
o_WRONLY :: CInt
o_WRONLY = o_WRONLY
o_RDWR   :: CInt
o_RDWR = o_RDWR
o_APPEND :: CInt
o_APPEND = o_APPEND
o_CREAT  :: CInt
o_CREAT = o_CREAT
o_EXCL   :: CInt
o_EXCL = o_EXCL
o_TRUNC  :: CInt
o_TRUNC = o_TRUNC

-- non-POSIX flags.
o_NOCTTY   :: CInt
o_NOCTTY = o_NOCTTY
o_NONBLOCK :: CInt
o_NONBLOCK = o_NONBLOCK
o_BINARY   :: CInt
o_BINARY = o_BINARY

c_s_isreg  :: CMode -> CInt
c_s_isreg = c_s_isreg
c_s_ischr  :: CMode -> CInt
c_s_ischr = c_s_ischr
c_s_isblk  :: CMode -> CInt
c_s_isblk = c_s_isblk
c_s_isdir  :: CMode -> CInt
c_s_isdir = c_s_isdir
c_s_isfifo :: CMode -> CInt
c_s_isfifo = c_s_isfifo

s_isreg  :: CMode -> Bool
s_isreg cm = c_s_isreg cm /= 0
s_ischr  :: CMode -> Bool
s_ischr cm = c_s_ischr cm /= 0
s_isblk  :: CMode -> Bool
s_isblk cm = c_s_isblk cm /= 0
s_isdir  :: CMode -> Bool
s_isdir cm = c_s_isdir cm /= 0
s_isfifo :: CMode -> Bool
s_isfifo cm = c_s_isfifo cm /= 0

sizeof_stat :: Int
sizeof_stat = sizeof_stat
st_mtime :: Ptr CStat -> IO CTime
st_mtime = st_mtime
#ifdef mingw32_HOST_OS
st_size :: Ptr CStat -> IO Int64
st_size = st_size
#else
st_size :: Ptr CStat -> IO COff
st_size = st_size
#endif
st_mode :: Ptr CStat -> IO CMode
st_mode = st_mode
st_dev :: Ptr CStat -> IO CDev
st_dev = st_dev
st_ino :: Ptr CStat -> IO CIno
st_ino = st_ino

const_echo :: CInt
const_echo = const_echo
const_tcsanow :: CInt
const_tcsanow = const_tcsanow
const_icanon :: CInt
const_icanon = const_icanon
const_vmin   :: CInt
const_vmin = const_vmin
const_vtime  :: CInt
const_vtime = const_vtime
const_sigttou :: CInt
const_sigttou = const_sigttou
const_sig_block :: CInt
const_sig_block = const_sig_block
const_sig_setmask :: CInt
const_sig_setmask = const_sig_setmask
const_f_getfl :: CInt
const_f_getfl = const_f_getfl
const_f_setfl :: CInt
const_f_setfl = const_f_setfl
const_f_setfd :: CInt
const_f_setfd = const_f_setfd
const_fd_cloexec :: CLong
const_fd_cloexec = const_fd_cloexec

#if defined(HTYPE_TCFLAG_T)
sizeof_termios :: Int
sizeof_termios = sizeof_termios
sizeof_sigset_t :: Int
sizeof_sigset_t = sizeof_sigset_t

c_lflag :: Ptr CTermios -> IO CTcflag
c_lflag = c_lflag
poke_c_lflag :: Ptr CTermios -> CTcflag -> IO ()
poke_c_lflag = poke_c_lflag
ptr_c_cc  :: Ptr CTermios -> IO (Ptr Word8)
ptr_c_cc = ptr_c_cc
#endif

s_issock :: CMode -> Bool
#if !defined(mingw32_HOST_OS)
s_issock cmode = c_s_issock cmode /= 0
c_s_issock :: CMode -> CInt
c_s_issock = c_s_issock
#else
s_issock _ = False
#endif

dEFAULT_BUFFER_SIZE :: Int
dEFAULT_BUFFER_SIZE = dEFAULT_BUFFER_SIZE
sEEK_CUR :: CInt
sEEK_CUR = sEEK_CUR
sEEK_SET :: CInt
sEEK_SET = sEEK_SET
sEEK_END :: CInt
sEEK_END = sEEK_END

{-
Note: Windows types

Windows' _read and _write have types that differ from POSIX. They take an
unsigned int for lengh and return a signed int where POSIX uses size_t and
ssize_t. Those are different on x86_64 and equivalent on x86. We import them
with the types in Microsoft's documentation which means that c_read,
c_safe_read, c_write and c_safe_write have different Haskell types depending on
the OS.
-}


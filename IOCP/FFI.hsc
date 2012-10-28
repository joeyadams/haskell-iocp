{-# LANGUAGE CPP #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module IOCP.FFI (
    -- * IOCP
    IOCP(..),
    newIOCP,
    associateHandleWithIOCP,
    getNextCompletion,

    -- * Overlapped
    Overlapped(..),
    newOverlapped,
    discardOverlapped,

    -- * Cancel pending I/O
    cancelIo,

    -- * Miscellaneous
    throwWinErr,
) where

#include <windows.h>

##ifdef mingw32_HOST_OS
## if defined(i386_HOST_ARCH)
##  define WINDOWS_CCONV stdcall
## elif defined(x86_64_HOST_ARCH)
##  define WINDOWS_CCONV ccall
## else
##  error Unknown mingw32 arch
## endif
##endif

import Data.Word
import Foreign
import System.Win32.Types

import qualified System.Win32.Types as Win32

------------------------------------------------------------------------
-- IOCP

-- |
--
-- The type variable @a@ represents additional data each completion carries
-- with it.  After associating a 'HANDLE' with a completion port, you must not
-- initiate IO on it with @OVERLAPPED@ structures other than those created by
-- 'newOverlapped' with the same type.
newtype IOCP a = IOCP HANDLE
    deriving (Eq, Ord, Show)

foreign import WINDOWS_CCONV unsafe "windows.h CreateIoCompletionPort"
    c_CreateIoCompletionPort :: HANDLE -> IOCP a -> ULONG_PTR -> DWORD -> IO (IOCP a)

newIOCP :: IO (IOCP a)
newIOCP =
    Win32.failIf (== IOCP nullPtr) "newIOCP" $
        c_CreateIoCompletionPort iNVALID_HANDLE_VALUE (IOCP nullPtr) 0 1

associateHandleWithIOCP :: IOCP a -> HANDLE -> IO ()
associateHandleWithIOCP iocp handle =
    Win32.failIf_ (/= iocp) "associateHandleWithIOCP" $
        c_CreateIoCompletionPort handle iocp 0 0

foreign import ccall safe
    c_iocp_get_next_completion
        :: IOCP a -> DWORD
        -> Ptr DWORD -> Ptr DWORD -> Ptr (StablePtr a) -> IO BOOL

getNextCompletion :: IOCP a
                  -> DWORD  -- ^ Timeout in milliseconds (or 'GHC.Windows.iNFINITE')
                  -> IO (Maybe (a, DWORD, ErrCode))
getNextCompletion iocp timeout =
    alloca $ \num_bytes_ptr ->
    alloca $ \err_ptr ->
    alloca $ \userdata_ptr -> do
        ok <- c_iocp_get_next_completion
                  iocp timeout
                  num_bytes_ptr err_ptr userdata_ptr
        err <- peek err_ptr
        if ok then do
            num_bytes <- peek num_bytes_ptr
            a         <- peek userdata_ptr >>= takeStablePtr
            return $ Just (a, num_bytes, err)
        else if err == #{const WAIT_TIMEOUT} then
            return Nothing
        else
            Win32.failWith "GetQueuedCompletionStatus" err

------------------------------------------------------------------------
-- Overlapped

-- | Identifies an I/O operation, and delivers a value of type @a@
-- to the completion port.
newtype Overlapped a = Overlapped (Ptr ())
    deriving (Eq, Ord, Show)

foreign import ccall unsafe
    c_iocp_new_overlapped :: Word64 -> StablePtr a -> IO (Overlapped a)

-- | Allocate a new
-- <http://msdn.microsoft.com/en-us/library/windows/desktop/ms684342%28v=vs.85%29.aspx OVERLAPPED>
-- structure.  The resulting pointer may be passed to a system call that takes
-- an @LPOVERLAPPED@, provided the @HANDLE@ was associated with an 'IOCP' with
-- the same type @a@.
newOverlapped :: Word64 -- ^ Offset/OffsetHigh
              -> a      -- ^ Application context (stored outside the @OVERLAPPED@ structure)
              -> IO (Overlapped a)
newOverlapped offset ctx = do
    ptr <- newStablePtr ctx
    Win32.failIf (== Overlapped nullPtr) "newOverlapped" $
        c_iocp_new_overlapped offset ptr

foreign import ccall unsafe
    c_iocp_finish_overlapped :: Overlapped a -> IO (StablePtr a)

-- | Discard an 'Overlapped' object.  This should be called if and only if
-- no pending I/O was produced after all.
discardOverlapped :: Overlapped a -> IO ()
discardOverlapped o = c_iocp_finish_overlapped o >>= freeStablePtr

------------------------------------------------------------------------
-- Cancel pending I/O

-- | CancelIo shouldn't block, but cancellation happens infrequently,
-- so we might as well be on the safe side.
foreign import WINDOWS_CCONV safe "windows.h CancelIo"
    c_CancelIo :: HANDLE -> IO BOOL

-- | Cancel all pending overlapped I/O for the given file that was initiated by
-- the current OS thread.
cancelIo :: HANDLE -> IO ()
cancelIo = Win32.failIfFalse_ "CancelIo" . c_CancelIo

------------------------------------------------------------------------
-- Miscellaneous

type ULONG_PTR = #type ULONG_PTR

takeStablePtr :: StablePtr a -> IO a
takeStablePtr ptr = do
    a <- deRefStablePtr ptr
    freeStablePtr ptr
    return a

throwWinErr :: String -> ErrCode -> IO a
throwWinErr loc err = do
    c_SetLastError err
    Win32.failWith loc err

foreign import WINDOWS_CCONV unsafe "windows.h SetLastError"
    c_SetLastError :: ErrCode -> IO ()

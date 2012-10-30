{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Winsock (
    Socket,
    socket,
    connect,
    close,
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

import IOCP.Manager             (IOCPHandle, LPOVERLAPPED)
import qualified IOCP.FFI     as FFI
import qualified IOCP.Manager as Manager

import Control.Applicative      ((<$>))
import Control.Exception
import Control.Concurrent.MVar
import Control.Monad            (join, void)
import Data.IORef
import Data.Word
import Foreign.C
import Foreign.Ptr
import Network.Socket.Internal  (withSockAddr)
import System.IO.Unsafe         (unsafePerformIO)
import System.Win32.Types

import qualified Network.Socket     as NS
import qualified System.Win32.Types as Win32

newtype Socket = Socket IOCPHandle
    deriving Eq

socket :: NS.Family -> NS.SocketType -> NS.ProtocolNumber -> IO Socket
socket family stype protocol = do
    initWinsock
    sockH <- wordPtrToPtr . fromIntegral . NS.fdSocket
         <$> NS.socket family stype protocol
    Just mgr <- Manager.getSystemManager
    Socket <$> Manager.associate mgr sockH

connect :: Socket -> NS.SockAddr -> IO ()
connect (Socket ih) addr =
    mask_ $ do
        winsock <- getWinsock
        mv <- newEmptyMVar

        let startCB h overlapped =
                withSockAddr addr $ \addr_ptr addr_len ->
                Win32.failIfFalse_ "connect" $
                c_winsock_connect winsock (castHANDLEToSOCKET h)
                                  addr_ptr (fromIntegral addr_len)
                                  overlapped

            completionCB err _numBytes
                | err == 0  = tryPutMVar_ mv $ return ()
                | otherwise = FFI.throwWinErr "connect" err

            errorCB = tryPutMVar_ mv . throwIO

        job <- Manager.startJob ih 0 startCB completionCB errorCB
        join (takeMVar mv `onException` Manager.cancelJob job)

close :: Socket -> IO ()
close (Socket ih) =
    Manager.closeWith ih $
    Win32.failIf_ (/= 0) "close" . c_closesocket . castHANDLEToSOCKET

newtype Winsock = Winsock (Ptr ())

getWinsock :: IO Winsock
getWinsock = readIORef winsockRef

initWinsock :: IO ()
initWinsock = void getWinsock

winsockRef :: IORef Winsock
winsockRef = unsafePerformIO (c_winsock_init >>= newIORef)
{-# NOINLINE winsockRef #-}

type SOCKET = #type SOCKET

-- castSOCKETToHANDLE :: SOCKET -> HANDLE
-- castSOCKETToHANDLE = wordPtrToPtr . fromIntegral

castHANDLEToSOCKET :: HANDLE -> SOCKET
castHANDLEToSOCKET = fromIntegral . ptrToWordPtr

foreign import ccall unsafe
    c_winsock_init :: IO Winsock

foreign import ccall unsafe
    c_winsock_connect :: Winsock -> SOCKET -> Ptr NS.SockAddr -> CInt -> LPOVERLAPPED -> IO BOOL

foreign import WINDOWS_CCONV safe "winsock2.h closesocket"
    c_closesocket :: SOCKET -> IO CInt

tryPutMVar_ :: MVar a -> a -> IO ()
tryPutMVar_ mv x = void (tryPutMVar mv x)

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

data Socket = Socket
    { _socketIOCP :: !IOCPHandle
    ,  socketSock :: !SOCKET
    }
    deriving Eq

socket :: NS.Family -> NS.SocketType -> NS.ProtocolNumber -> IO Socket
socket family stype protocol = do
    initWinsock
    sock <- NS.fdSocket <$> NS.socket family stype protocol
    Just mgr <- Manager.getSystemManager
    iocp <- Manager.associate mgr (wordPtrToPtr $ fromIntegral sock)
    return $ Socket iocp (fromIntegral sock)

connect :: Socket -> NS.SockAddr -> IO ()
connect (Socket ih sock) addr =
    mask_ $ do
        winsock <- getWinsock
        mv <- newEmptyMVar
        let startCB _h overlapped =
                withSockAddr addr $ \addr_ptr addr_len -> do
                    ok <- c_winsock_connect winsock sock
                                            addr_ptr (fromIntegral addr_len)
                                            overlapped
                    if ok then
                        return True
                    else do
                        err <- Win32.getLastError
                        tryPutMVar_ mv $ FFI.throwWinErr "connect" err
                        return False
            completionCB err _numBytes
                | err == 0  = tryPutMVar_ mv $ return ()
                | otherwise = tryPutMVar_ mv $ FFI.throwWinErr "connect" err
        job <- Manager.startJob ih 0 startCB completionCB
        join (takeMVar mv `onException` Manager.cancelJob job)

close :: Socket -> IO ()
close = Win32.failIf_ (/= 0) "close" . c_closesocket . socketSock

newtype Winsock = Winsock (Ptr ())

getWinsock :: IO Winsock
getWinsock = readIORef winsockRef

initWinsock :: IO ()
initWinsock = void getWinsock

winsockRef :: IORef Winsock
winsockRef = unsafePerformIO (c_winsock_init >>= newIORef)
{-# NOINLINE winsockRef #-}

type SOCKET = #type SOCKET

foreign import ccall unsafe
    c_winsock_init :: IO Winsock

foreign import ccall unsafe
    c_winsock_connect :: Winsock -> SOCKET -> Ptr NS.SockAddr -> CInt -> LPOVERLAPPED -> IO BOOL

foreign import WINDOWS_CCONV safe "winsock2.h closesocket"
    c_closesocket :: SOCKET -> IO CInt

tryPutMVar_ :: MVar a -> a -> IO ()
tryPutMVar_ mv x = void (tryPutMVar mv x)

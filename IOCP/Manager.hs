{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module IOCP.Manager (
    -- * Manager
    Manager,
    new,
    getSystemManager,

    -- * IOCPHandle
    IOCPHandle,
    associate,
    withHANDLE,
    closeWith,

    -- * Performing overlapped I/O
    withIOCP,
    StartCallback,
    CompletionCallback,
    LPOVERLAPPED,
) where

import IOCP.Worker (Worker, forkOSUnmasked)
import qualified IOCP.FFI    as FFI
import qualified IOCP.Worker as Worker

import Control.Concurrent
import Control.Exception as E
import Control.Monad        (forever, join, void)
import Data.IORef
import Data.Word            (Word64)
import Foreign.Ptr          (Ptr)
import GHC.Windows          (iNFINITE)
import System.IO.Unsafe     (unsafeInterleaveIO, unsafePerformIO)
import System.Win32.Types   (DWORD, ErrCode, HANDLE)

data Manager = Manager
    { managerCompletionPort :: !(FFI.IOCP (CompletionCallback ()))
    , managerThreadPool     :: WorkerList
    }

data WorkerList = WL !Worker WorkerList

new :: IO Manager
new = do
    managerCompletionPort <- FFI.newIOCP
    managerThreadPool     <- newThreadPool
    _tid <- forkOSUnmasked $ forever $ do
        m <- FFI.getNextCompletion managerCompletionPort iNFINITE
        case m of
            Nothing ->
                fail "getNextCompletion unexpectedly timed out"
            Just (cb, numBytes, errCode) ->
                cb errCode numBytes
    return Manager{..}

getSystemManager :: IO (Maybe Manager)
getSystemManager = readIORef managerRef

managerRef :: IORef (Maybe Manager)
managerRef = unsafePerformIO $
    if rtsSupportsBoundThreads
        then new >>= newIORef . Just
        else newIORef Nothing
{-# NOINLINE managerRef #-}

-- | Nifty trick to allow each 'IOCPHandle' to allocate workers per concurrent
-- task, while allowing all 'IOCPHandles' to share the thread pool as a whole.
newThreadPool :: IO WorkerList
newThreadPool = unsafeInterleaveIO $ do
    w  <- Worker.new
    ws <- newThreadPool
    return (WL w ws)
{-# NOINLINE newThreadPool #-}

associate :: Manager -> HANDLE -> IO IOCPHandle
associate Manager{..} h = do
    FFI.associateHandleWithIOCP managerCompletionPort h
    iHandle <- newMVar $ IOCPOpen h
    iPool   <- newIORef managerThreadPool
    return IOCPHandle{..}

data IOCPHandle = IOCPHandle
    { iHandle :: !(MVar IOCPState)
    , iPool   :: !(IORef WorkerList)
    }

data IOCPState = IOCPOpen !HANDLE
               | IOCPClosed

instance Eq IOCPHandle where
    (==) (IOCPHandle a _) (IOCPHandle b _) = a == b

withHANDLE' :: (IOCPState -> IOCPState) -> IOCPHandle -> (HANDLE -> IO a) -> IO (Maybe a)
withHANDLE' f IOCPHandle{..} cb =
    mask_ $ do
        m <- takeMVar iHandle
        case m of
            IOCPOpen h -> do
                a <- cb h `onException` putMVar iHandle m
                putMVar iHandle (f m)
                return (Just a)
            IOCPClosed -> do
                putMVar iHandle m
                return Nothing

-- | Operate on the underlying 'HANDLE', or do nothing and return 'Nothing' if
-- the handle is closed.
withHANDLE :: IOCPHandle -> (HANDLE -> IO a) -> IO (Maybe a)
withHANDLE h = withHANDLE' id h

-- | Close the underlying 'HANDLE' using the given callback.  Afterward,
-- 'withIOCP' will throw an exception, and 'closeWith' will do nothing.
closeWith :: IOCPHandle -> (HANDLE -> IO ()) -> IO ()
closeWith h = void . withHANDLE' (\_ -> IOCPClosed) h

withWorkerAndMask :: IOCPHandle -> (Worker -> IO a) -> IO a
withWorkerAndMask IOCPHandle{..} cb =
    mask_ $ do
        w <- atomicModifyIORef iPool (\(WL w ws) -> (ws, w))
        let release = atomicModifyIORef iPool (\ws -> (WL w ws, ()))
        a <- (evaluate w >> cb w) `onException` release
        release
        return a

type LPOVERLAPPED = Ptr ()

-- | Callback that starts the overlapped I/O operation.
-- It must return successfully if and only if an I/O completion has been
-- queued.  Otherwise, it must throw an exception.  This exception will be
-- rethrown by 'withIOCP'.
type StartCallback = HANDLE -> LPOVERLAPPED -> IO ()

-- | Callback (called from the I/O manager) for translating a completion.
-- Any exception it throws will be rethrown by 'withIOCP'.
type CompletionCallback a = ErrCode   -- ^ 0 indicates success
                         -> DWORD     -- ^ Number of bytes transferred
                         -> IO a

withIOCP :: IOCPHandle
         -> Word64                  -- ^ Offset/OffsetHigh
         -> StartCallback
         -> CompletionCallback a
         -> IO a
withIOCP ih@IOCPHandle{..} offset startCB completionCB =
    withWorkerAndMask ih $ \w -> do
        signal <- newEmptyMVar
        let signalReturn a = void $ tryPutMVar signal $ return a
            signalThrow ex = void $ tryPutMVar signal $ throwIO (ex :: SomeException)

        Worker.enqueue w $ do
            s <- takeMVar iHandle
            case s of
                IOCPClosed -> do
                    putMVar iHandle s
                    signalThrow $ toException closedError
                IOCPOpen h -> do
                    let completionCB' :: CompletionCallback ()
                        completionCB' e b =
                            (completionCB e b >>= signalReturn) `E.catch` signalThrow

                    ol@(FFI.Overlapped ptr) <- FFI.newOverlapped offset completionCB'
                    res <- try $ startCB h ptr
                    putMVar iHandle s
                    case res of
                        Left ex -> do
                            FFI.discardOverlapped ol
                            signalThrow ex
                        Right _ -> return ()

        let cancel = uninterruptibleMask_ $ do
                Worker.enqueue w $ void $ withHANDLE ih FFI.cancelIo
                takeMVar signal

        join (takeMVar signal `onException` cancel)

closedError :: IOError
closedError = userError "HANDLE closed"

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
    iHANDLE,

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
    { managerCompletionPort :: !ManagerCompletionPort
    , managerThreadPool     :: WorkerList
    }

type ManagerCompletionPort = FFI.IOCP (CompletionCallback ())

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
    iPool <- newIORef $ PoolState (Just managerCompletionPort) managerThreadPool
    return IOCPHandle{iHandle = h, ..}

data IOCPHandle = IOCPHandle
    { iHandle   :: !HANDLE
    , iPool     :: !(IORef PoolState)
    }

instance Eq IOCPHandle where
    (==) (IOCPHandle _ a) (IOCPHandle _ b) = a == b

-- | Get the underlying 'HANDLE'.
iHANDLE :: IOCPHandle -> HANDLE
iHANDLE = iHandle

-- | Used to allocate worker threads for I/O requests.  The rule is: a handle
-- may not use the same worker for two simultaneous operations (however,
-- multiple handles may share the same worker).  This is because CancelIo
-- cancels all pending I/O for a given handle in the current thread.
-- CancelIoEx lets us specify an individual operation to cancel, but it was
-- introduced in Windows Vista.
--
-- Whenever we can, we queue jobs to the completion handler using
-- PostQueuedCompletionStatus.  This is about 30% faster than using a separate
-- worker thread, as it avoids a context switch.
data PoolState = PoolState
    { pCompletionPort :: !(Maybe ManagerCompletionPort)
    , pWorkers        :: WorkerList
    }

withWorkerAndMask :: IOCPHandle -> ((IO () -> IO ()) -> IO a) -> IO a
withWorkerAndMask IOCPHandle{..} =
    withIORefAndMask grab iPool
  where
    grab ps@PoolState{..} = case pCompletionPort of
        Just cp -> (ps{pCompletionPort = Nothing}, (releaseCP cp, postCP cp))
        Nothing -> case pWorkers of
            WL w ws -> (ps{pWorkers = ws}, (releaseWorker w, Worker.enqueue w))

    releaseCP cp ps = ps{pCompletionPort = Just cp}
    releaseWorker w ps = ps{pWorkers = WL w (pWorkers ps)}

    postCP cp io = FFI.newOverlapped 0 (\_errCode _numBytes -> io)
               >>= FFI.postCompletion cp 0

withIORefAndMask :: (s -> (s, (s -> s, a))) -> IORef s -> (a -> IO b) -> IO b
withIORefAndMask grabF ref cb =
    mask_ $ do
        (releaseF, a) <- atomicModifyIORef ref grabF
        let release = atomicModifyIORef ref (\s -> (releaseF s, ()))
        b <- cb a `onException` release
        release
        return b

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

-- |
--
-- The underlying 'HANDLE' must not be closed while 'withIOCP' is in progress.
withIOCP :: IOCPHandle
         -> Word64                  -- ^ Offset/OffsetHigh
         -> StartCallback
         -> CompletionCallback a
         -> IO a
withIOCP ih@IOCPHandle{..} offset startCB completionCB =
    withWorkerAndMask ih $ \enqueue -> do
        signal <- newEmptyMVar
        let signalReturn a = void $ tryPutMVar signal $ return a
            signalThrow ex = void $ tryPutMVar signal $ throwIO (ex :: SomeException)

        enqueue $ do
            let completionCB' :: CompletionCallback ()
                completionCB' e b =
                    (completionCB e b >>= signalReturn) `E.catch` signalThrow

            ol@(FFI.Overlapped ptr) <- FFI.newOverlapped offset completionCB'

            startCB iHandle ptr `E.catch` \ex -> do
                FFI.discardOverlapped ol
                signalThrow ex

        let cancel = uninterruptibleMask_ $ do
                enqueue $ FFI.cancelIo iHandle
                takeMVar signal

        join (takeMVar signal `onException` cancel)

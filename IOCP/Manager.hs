{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module IOCP.Manager (
    -- * Manager
    Manager,
    new,
    getSystemManager,

    -- * Handle registration
    registerHandle,
    closeHandleWith,

    -- * Performing overlapped I/O
    withOverlapped,
    StartCallback,
    CompletionCallback,
    LPOVERLAPPED,
) where

import IOCP.Worker (Worker, forkOSUnmasked)
import qualified IOCP.FFI    as FFI
import qualified IOCP.Worker as Worker

import Control.Concurrent
import Control.Exception as E
import Control.Monad        (forever, join, void, when)
import Data.IORef
import Data.Word            (Word64)
import Debug.Trace          (traceIO)
import Foreign.Ptr          (Ptr, ptrToIntPtr)
import GHC.Windows          (iNFINITE)
import System.IO.Unsafe     (unsafeInterleaveIO, unsafePerformIO)
import System.Win32.Types   (DWORD, ErrCode, HANDLE)

import qualified Data.IntMap as IM

data Manager = Manager
    { managerCompletionPort :: !ManagerCompletionPort
    , managerHandles        :: !(IORef (IM.IntMap HandleState))
    , managerThreadPool     :: WorkerList
    }

instance Eq Manager where
    (==) a b = managerHandles a == managerHandles b

type ManagerCompletionPort = FFI.IOCP (CompletionCallback ())

data HandleState = HandleState
    { hsPool    :: !(IORef PoolState)
    , hsClosed  :: !(MVar Bool)
    }

instance Eq HandleState where
    (==) a b = hsPool a == hsPool b

data WorkerList = WL !Worker WorkerList

new :: IO Manager
new = do
    managerCompletionPort <- FFI.newIOCP
    managerHandles        <- newIORef IM.empty
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

-- | Nifty trick to allow each 'HandleState' to allocate workers per concurrent
-- task, while allowing all 'HandleState's to share the thread pool as a whole.
newThreadPool :: IO WorkerList
newThreadPool = unsafeInterleaveIO $ do
    w  <- Worker.new
    ws <- newThreadPool
    return (WL w ws)
{-# NOINLINE newThreadPool #-}

handleToInt :: HANDLE -> Int
handleToInt = fromIntegral . ptrToIntPtr

-- | Register a 'HANDLE' with the I/O manager.  This must be done before using
-- the handle with 'withOverlapped'.
--
-- To close the handle, you must use 'closeHandleWith'.
registerHandle :: Manager -> HANDLE -> IO ()
registerHandle mgr@Manager{..} h = do
    hstate <- do
        hsPool   <- newIORef $ PoolState True managerThreadPool
        hsClosed <- newMVar False
        return HandleState{..}
    mask_ $ join $ atomicModifyIORef managerHandles $ \m ->
        case IM.lookup key m of
            Nothing -> let !m' = IM.insert key hstate m
                        in (m', associate `onException` unregister mgr key hstate)
            Just _ -> (m, fail "IOCP.Manager.registerHandle: handle already registered")
  where
    key = handleToInt h
    associate = FFI.associateHandleWithIOCP managerCompletionPort h

-- | Remove an entry from managerHandles.  Guard against a potential race
-- condition where, after one HANDLE is closed, another one is created with the
-- same address.
unregister :: Manager -> Int -> HandleState -> IO ()
unregister Manager{..} key hstate = do
    !() <- atomicModifyIORef managerHandles $ \m ->
        case IM.lookup key m of
            Just hstate' | hstate == hstate' ->
                let !m' = IM.delete key m
                 in (m', ())
            _ -> (m, ())
    return ()

-- | Close the 'HANDLE' and unregister it from the I/O manager.
-- The callback is run in an exception 'mask', and should not use interruptible
-- operations (e.g. 'takeMVar', 'threadDelay', 'System.IO.hClose').
closeHandleWith :: Manager -> (HANDLE -> IO ()) -> HANDLE -> IO ()
closeHandleWith Manager{..} close h =
    mask_ $ do
        HandleState{..} <-
            join $ atomicModifyIORef managerHandles $ \m ->
            case IM.lookup key m of
                Nothing     -> (m, failNotRegistered)
                Just hstate -> let !m' = IM.delete key m
                                in (m', return hstate)
        closed <- takeMVar hsClosed
        -- closed should be False
        close h `onException` putMVar hsClosed closed
        putMVar hsClosed True
  where
    key = handleToInt h
    failNotRegistered = fail "IOCP.Manager.closeHandleWith: handle already closed, or not registered"

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
    { pCompletionPort :: !Bool
    , pWorkers        :: WorkerList
    }

withWorkerAndMask :: Manager -> IORef PoolState -> ((IO () -> IO ()) -> IO a) -> IO a
withWorkerAndMask mgr =
    withIORefAndMask grab
  where
    grab ps@PoolState{..} =
        if pCompletionPort then
            (ps{pCompletionPort = False}, (releaseCP, postWorkToCP mgr))
        else
            case pWorkers of
                WL w ws -> (ps{pWorkers = ws}, (releaseWorker w, Worker.enqueue w))

    releaseCP       ps = ps{pCompletionPort = True}
    releaseWorker w ps = ps{pWorkers = WL w (pWorkers ps)}

withIORefAndMask :: (s -> (s, (s -> s, a))) -> IORef s -> (a -> IO b) -> IO b
withIORefAndMask grabF ref cb =
    mask_ $ do
        (releaseF, a) <- atomicModifyIORef ref grabF
        let release = atomicModifyIORef ref (\s -> (releaseF s, ()))
        b <- cb a `onException` release
        release
        return b

postWorkToCP :: Manager -> IO () -> IO ()
postWorkToCP Manager{..} io =
    FFI.newOverlapped 0 completionCB >>= FFI.postCompletion managerCompletionPort 0
  where
    completionCB :: CompletionCallback ()
    completionCB _errCode _numBytes = io

type LPOVERLAPPED = Ptr ()

-- | Callback that starts the overlapped I/O operation.
-- It must return successfully if and only if an I/O completion has been
-- queued.  Otherwise, it must throw an exception.  This exception will be
-- rethrown by 'withOverlapped'.
type StartCallback = LPOVERLAPPED -> IO ()

-- | Callback (called from the I/O manager) for translating a completion.
-- Any exception it throws will be rethrown by 'withOverlapped'.
type CompletionCallback a = ErrCode   -- ^ 0 indicates success
                         -> DWORD     -- ^ Number of bytes transferred
                         -> IO a

-- | Start an overlapped I/O operation, and wait for its completion.  If
-- 'withOverlapped' is interrupted by an asynchronous exception, the operation
-- will be canceled using @CancelIo@.
--
-- 'withOverlapped' waits for a completion to arrive before returning or
-- throwing an exception.  This means you can use functions like
-- 'Foreign.Marshal.Alloc.alloca' to allocate buffers for the operation.
withOverlapped :: Manager
               -> HANDLE
               -> Word64 -- ^ Value to use for the @OVERLAPPED@
                         --   structure's Offset/OffsetHigh members.
               -> StartCallback
               -> CompletionCallback a
               -> IO a
withOverlapped mgr h offset startCB completionCB = do
    hstate <- readIORef (managerHandles mgr)
          >>= maybe failNotRegistered return . IM.lookup (handleToInt h)

    withWorkerAndMask mgr (hsPool hstate) $ \enqueue -> do
        signal <- newEmptyMVar
        let signalReturn a = void $ tryPutMVar signal $ return a
            signalThrow ex = void $ tryPutMVar signal $ throwIO (ex :: SomeException)

        enqueue $ do
            let completionCB' :: CompletionCallback ()
                completionCB' e b =
                    (completionCB e b >>= signalReturn) `E.catch` signalThrow

            e <- try $ FFI.newOverlapped offset completionCB'
            case e of
                Left ex -> signalThrow ex
                Right ol@(FFI.Overlapped ptr) ->
                    startCB ptr `E.catch` \ex -> do
                        FFI.discardOverlapped ol
                        signalThrow ex

        let cancel = uninterruptibleMask_ $ do
                enqueue $ do
                    -- NOTE: This may run after 'withOverlapped' has completed.
                    -- The 'hsClosed' lock is how we keep the handle from being
                    -- closed under us.
                    closed <- takeMVar (hsClosed hstate)
                    when (not closed) $
                        FFI.cancelIo h `E.catch` \ex -> do
                            traceIO $ "CancelIo failed: " ++ show (ex :: SomeException)
                            signalThrow ex
                    putMVar (hsClosed hstate) closed
                takeMVar signal

        join (takeMVar signal `onException` cancel)

  where
    failNotRegistered = fail "IOCP.Manager.withOverlapped: handle closed or not registered"

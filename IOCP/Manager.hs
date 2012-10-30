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
    Job,
    startJob,
    cancelJob,

    -- ** Types
    StartCallback,
    CompletionCallback,
    ErrorCallback,
    LPOVERLAPPED,
) where

import IOCP.Worker (Worker, forkOSUnmasked)
import qualified IOCP.FFI    as FFI
import qualified IOCP.Worker as Worker

import Control.Concurrent
import Control.Exception as E
import Control.Monad        (forever, void)
import Data.IORef
import Data.Word            (Word64)
import Foreign.Ptr          (Ptr)
import GHC.Windows          (iNFINITE)
import System.IO.Unsafe     (unsafeInterleaveIO, unsafePerformIO)
import System.Win32.Types   (DWORD, ErrCode, HANDLE)

data Manager = Manager
    { managerCompletionPort :: !(FFI.IOCP CompletionCallback)
    , managerThreadPool     :: [Worker]
    }

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
newThreadPool :: IO [Worker]
newThreadPool = unsafeInterleaveIO $ do
    w  <- Worker.new
    ws <- newThreadPool
    return (w:ws)

grabWorker :: IORef [Worker] -> IO Worker
grabWorker ref = atomicModifyIORef ref (\(x:xs) -> (xs, x))

releaseWorker :: IORef [Worker] -> Worker -> IO ()
releaseWorker ref x = atomicModifyIORef ref (\xs -> (x:xs, ()))

associate :: Manager -> HANDLE -> IO IOCPHandle
associate Manager{..} h = do
    FFI.associateHandleWithIOCP managerCompletionPort h
    iHandle <- newMVar (Just h)
    iPool   <- newIORef managerThreadPool
    return IOCPHandle{..}

withHANDLE' :: (Maybe HANDLE -> Maybe HANDLE) -> IOCPHandle -> (HANDLE -> IO a) -> IO (Maybe a)
withHANDLE' f IOCPHandle{..} cb =
    mask_ $ do
        m <- takeMVar iHandle
        case m of
            Nothing -> do
                putMVar iHandle m
                return Nothing
            Just h -> do
                a <- cb h `onException` putMVar iHandle m
                putMVar iHandle (f m)
                return (Just a)

-- | Operate on the underlying 'HANDLE', or do nothing and return 'Nothing' if
-- the handle is closed.
withHANDLE :: IOCPHandle -> (HANDLE -> IO a) -> IO (Maybe a)
withHANDLE h = mask_ . withHANDLE' id h

-- | Close the underlying 'HANDLE' using the given callback.  Afterward,
-- 'startJob' will result in a call to the 'ErrorCallback', and 'closeWith'
-- will do nothing.
closeWith :: IOCPHandle -> (HANDLE -> IO ()) -> IO ()
closeWith h = void . mask_ . withHANDLE' (\_ -> Nothing) h

data IOCPHandle = IOCPHandle
    { iHandle :: !(MVar (Maybe HANDLE))
    , iPool   :: !(IORef [Worker])
    }

instance Eq IOCPHandle where
    (==) (IOCPHandle a _) (IOCPHandle b _) = a == b

data Job = Job
    { jobWorker :: !(IORef (Maybe Worker)) -- Used to ensure worker is only released once
    , jobHandle :: !IOCPHandle
    }

instance Eq Job where
    (==) (Job a _) (Job b _) = a == b

-- | Allocate a worker, and enqueue the given job.
newJob :: IOCPHandle -> (Job -> IO ()) -> IO Job
newJob h work =
    mask_ $ do
        w <- grabWorker (iPool h)
        ref <- newIORef (Just w)
        let job = Job ref h
        Worker.enqueue w (work job)
        return job

-- | If the 'Worker' has not been released yet, call the callback,
-- then release it.  Otherwise, do nothing.
--
-- This is only async exception safe if the callback does not do any
-- interruptible operations.
finishJob :: Job -> (Worker -> IO ()) -> IO ()
finishJob Job{..} cb =
    mask_ $ do
        m <- atomicModifyIORef jobWorker (\m -> (Nothing, m))
        case m of
            Nothing -> return ()
            Just w  -> cb w >> releaseWorker (iPool jobHandle) w

type LPOVERLAPPED = Ptr ()

-- | Must return successfully if and only if an I/O completion has been queued.
-- Otherwise, it must throw an exception.
type StartCallback = HANDLE -> LPOVERLAPPED -> IO ()

type CompletionCallback = ErrCode   -- ^ 0 indicates success
                       -> DWORD     -- ^ Number of bytes transferred
                       -> IO ()

-- | What to do if an error occurred.  This is called when:
--
--  * The 'StartCallback' or 'CompletionCallback' throws an exception.
--
--  * The 'IOCPHandle' is closed before a job can be started.
type ErrorCallback = SomeException -> IO ()

startJob :: IOCPHandle
         -> Word64              -- ^ Offset/OffsetHigh
         -> StartCallback
         -> CompletionCallback
         -> ErrorCallback
         -> IO Job
startJob ih !offset startCB completionCB !errorCB =
    newJob ih $ \job -> do
        ol@(FFI.Overlapped ptr) <- FFI.newOverlapped offset completionCB'
        let handleEx ex = do
                finishJob job (\_ -> return ())
                FFI.discardOverlapped ol
                errorCB ex
        r <- withHANDLE ih $ \h -> try $ startCB h ptr
        case r of
            Nothing         -> handleEx $ toException $ userError "startJob: Handle closed"
            Just (Left ex)  -> handleEx ex
            Just (Right ()) -> return ()
  where
    completionCB' :: CompletionCallback
    completionCB' e b = completionCB e b `E.catch` errorCB

cancelJob :: Job -> IO ()
cancelJob job =
    mask_ $
    finishJob job $ \w ->
    Worker.enqueue w $
    void $ withHANDLE (jobHandle job) $ \h ->
    FFI.cancelIo h

{-# LANGUAGE RecordWildCards #-}
module IOCP.Manager (
    -- * Manager
    Manager,
    new,
    getSystemManager,

    -- * IOCPHandle
    IOCPHandle,
    associate,

    -- * Performing overlapped I/O
    Job,
    startJob,
    cancelJob,

    -- ** Types
    StartCallback,
    CompletionCallback,
    LPOVERLAPPED,
) where

import IOCP.Worker (Worker, forkOSUnmasked)
import qualified IOCP.FFI    as FFI
import qualified IOCP.Worker as Worker

import Control.Concurrent
import Control.Exception
import Control.Monad        (forever, when)
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
    pool <- newIORef managerThreadPool
    return IOCPHandle
        { iHandle = h
        , iPool   = pool
        }

data IOCPHandle = IOCPHandle
    { iHandle :: !HANDLE
    , iPool   :: !(IORef [Worker])
    }

data Job = Job
    { jobWorker :: !(IORef (Maybe Worker)) -- Used to ensure worker is only released once
    , jobHandle :: !IOCPHandle
    }

instance Eq Job where
    (==) (Job a _) (Job b _) = a == b

-- | Allocate a worker, and enqueue the given job.
newJob :: IOCPHandle -> (Job -> IO ()) -> IO Job
newJob h work = do
    w <- grabWorker (iPool h)
    ref <- newIORef (Just w)
    let job = Job ref h
    Worker.enqueue w (work job)
    return job

-- | If the 'Worker' has not been released yet, call the callback,
-- then release it.  Otherwise, do nothing.
finishJob :: Job -> (Worker -> IO ()) -> IO ()
finishJob Job{..} cb = do
    m <- atomicModifyIORef jobWorker (\m -> (Nothing, m))
    case m of
        Nothing -> return ()
        Just w  -> cb w >> releaseWorker (iPool jobHandle) w

type LPOVERLAPPED = Ptr ()

-- | Must return 'True' if and only if an IO completion has been queued.
type StartCallback = HANDLE -> LPOVERLAPPED -> IO Bool

type CompletionCallback = ErrCode   -- ^ 0 indicates success
                       -> DWORD     -- ^ Number of bytes transferred
                       -> IO ()

startJob :: IOCPHandle
         -> Word64              -- ^ Offset/OffsetHigh
         -> StartCallback
         -> CompletionCallback
         -> IO Job
startJob h offset startCB completionCB =
    mask_ $
    newJob h $ \job -> do
        FFI.Overlapped ptr <- FFI.newOverlapped offset completionCB
        started <- startCB (iHandle h) ptr
        when (not started) $
            finishJob job (\_ -> return ())

cancelJob :: Job -> IO ()
cancelJob job =
    mask_ $
    finishJob job $ \w ->
    Worker.enqueue w $
    FFI.cancelIo $ iHandle $ jobHandle job

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module IOCP.ThreadPool (
    ThreadPool,
    new,
    enqueue,
) where

import Control.Applicative  ((<$>))
import Control.Concurrent
import Control.Exception
import Control.Monad        (forever, void)
import Data.IORef
import Data.Tuple           (swap)
import GHC.IO               (unsafeUnmask)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

newtype ThreadPool = ThreadPool (IORef (IntMap Worker))
    deriving Eq

new :: IO ThreadPool
new = ThreadPool <$> newIORef IntMap.empty

-- | Enqueue an action to the thread pool.  For any sequence of calls with the
-- same pool and thread number:
--
--  * The actions will run in the order they were enqueued.
--
--  * Each action will run in the same operating system thread.
--
-- 'enqueue' is not interruptible.
enqueue :: ThreadPool -> Int -> IO () -> IO ()
enqueue (ThreadPool ref) n io =
    mask_ $ do
        m <- IntMap.lookup n <$> readIORef ref
        case m of
            Just worker -> workerEnqueue worker io
            Nothing -> do
                worker <- newWorker
                m' <- atomicModifyIORef ref $ swap . insertOrLookup n worker
                -- If somebody else created a worker with this index, throw
                -- away the worker we just created and enqueue to the existing
                -- worker instead.
                case m' of
                    Nothing -> workerEnqueue worker io
                    Just existingWorker -> do
                        cancelWorker worker
                        workerEnqueue existingWorker io

-- | Insert a new pair, unless the key conflicts with an existing pair.
-- On conflict, return the existing value and leave the 'IntMap' intact.
insertOrLookup :: Int -> a -> IntMap a -> (Maybe a, IntMap a)
insertOrLookup = IntMap.insertLookupWithKey (\_key _new old -> old)

------------------------------------------------------------------------
-- Worker

data Worker = Worker
    { workerJobs :: !(IORef JobList)
    , workerWake :: !(MVar ())
    }

-- A difference list, but with (x >>) instead of (x :)
type JobList = IO () -> IO ()

-- | Append an action to the job list, so it will
-- run /after/ the existing actions.
snocJobList :: JobList -> IO () -> JobList
snocJobList dl io = dl . (io >>)

runJobList :: JobList -> IO ()
runJobList dl = dl (return ())

newWorker :: IO Worker
newWorker = do
    workerJobs <- newIORef id
    workerWake <- newEmptyMVar
    _ <- forkOSUnmasked $ forever $ do
        _ <- takeMVar workerWake
        Just jobs <- atomicModifyIORef workerJobs $ \jobs -> (id, Just jobs)
        runJobList jobs
    return Worker{..}

forkOSUnmasked :: IO () -> IO ThreadId
forkOSUnmasked = forkOS . unsafeUnmask

-- Should be run within 'mask'
workerEnqueue :: Worker -> IO () -> IO ()
workerEnqueue Worker{..} io = do
    !() <- atomicModifyIORef workerJobs $ \jobs -> (snocJobList jobs io, ())
    void $ tryPutMVar workerWake ()

-- Should be run within 'mask'
cancelWorker :: Worker -> IO ()
cancelWorker w = workerEnqueue w $ throwIO ThreadKilled

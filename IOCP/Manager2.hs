{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE RecordWildCards #-}
module IOCP.Manager2 (
    -- * Manager
    Manager,
    new,

    -- * Overlapped
    Overlapped(..),
    CompletionCallback,
    newOverlapped,
    discardOverlapped,

    -- * Posting to the completion port
    postOverlapped,
    postIO,

    -- * Timeouts
    TimeoutCallback,
    TimeoutKey,
    Seconds,
    registerTimeout,
    updateTimeout,
    unregisterTimeout,
) where

import IOCP.Clock   (Clock, Seconds, getClock, getTime)
import IOCP.FFI     (Overlapped(..))
import IOCP.Worker  (forkOSUnmasked)
import qualified IOCP.FFI as FFI
import qualified IOCP.PSQ as Q

import Data.Word
import Data.Unique
import System.Win32.Types

data Manager = Manager
    { mgrIOCP  :: !(FFI.IOCP ManagerCallback)
    , mgrClock :: !Clock
    }

type ManagerCallback = ErrCode -> DWORD -> Mgr ()

type TimeoutQueue = Q.PSQ TimeoutCallback

-- |
-- Warning: since the 'TimeoutCallback' is called from the I/O manager, it must
-- not throw an exception or block for a long period of time.  In particular,
-- be wary of 'Control.Exception.throwTo' and 'Control.Concurrent.killThread':
-- if the target thread is making a foreign call, these functions will block
-- until the call completes.
type TimeoutCallback = IO ()

newtype TimeoutKey = TK Unique
    deriving (Eq, Ord)

new :: IO Manager
new = do
    mgrIOCP  <- FFI.newIOCP
    mgrClock <- getClock
    let mgr = Manager{..}
    _tid <- forkOSUnmasked $ loop mgr
    return mgr

-- | Callback (called from the I/O manager) when the completion is delivered.
type CompletionCallback = ErrCode   -- ^ 0 indicates success
                       -> DWORD     -- ^ Number of bytes transferred
                       -> IO ()

newOverlapped :: Word64 -- ^ Offset/OffsetHigh
              -> CompletionCallback
              -> IO Overlapped
newOverlapped offset cb = newOverlappedEx offset (liftCompletionCallback cb)

liftCompletionCallback :: CompletionCallback -> ManagerCallback
liftCompletionCallback cb = \errCode numBytes -> liftIO $ cb errCode numBytes

-- | Internal variant of 'newOverlapped' that allows the callback to modify the
-- timeout queue.
newOverlappedEx :: Word64 -> ManagerCallback -> IO Overlapped
newOverlappedEx = FFI.newOverlapped

discardOverlapped :: Overlapped -> IO ()
discardOverlapped = FFI.discardOverlapped

postOverlapped :: Manager -> Overlapped -> IO ()
postOverlapped mgr = FFI.postCompletion (mgrIOCP mgr) 0

postMgr :: Manager -> Mgr () -> IO ()
postMgr mgr cb =
    newOverlappedEx 0 (\_errCode _numBytes -> cb) >>= postOverlapped mgr

-- | Queue an action to be performed by the I/O manager thread.
postIO :: Manager -> IO () -> IO ()
postIO mgr io =
    newOverlappedEx 0 (\_errCode _numBytes -> liftIO io) >>= postOverlapped mgr

-- | Register an action to be performed in the given number of seconds.  The
-- returned 'TimeoutKey' can be used to later unregister or update the timeout.
-- The timeout is automatically unregistered when it fires.
--
-- The 'TimeoutCallback' will not be called more than once.
registerTimeout :: Manager -> Seconds -> TimeoutCallback -> IO TimeoutKey
registerTimeout mgr relTime cb = do
    key <- newUnique
    now <- getTime (mgrClock mgr)
    let !expTime = now + relTime
    postMgr mgr $ modifyTQ $ Q.insert key expTime cb
    return $ TK key

-- | Update an active timeout to fire in the given number of seconds (from the
-- time 'updateTimeout' is called), instead of when it was going to fire.
-- This has no effect if the timeout has already fired.
updateTimeout :: Manager -> TimeoutKey -> Seconds -> IO ()
updateTimeout mgr (TK key) relTime = do
    now <- getTime (mgrClock mgr)
    let !expTime = now + relTime
    postMgr mgr $ modifyTQ $ Q.adjust (const expTime) key

-- | Unregister an active timeout.  This is a harmless no-op if the timeout is
-- already unregistered or has already fired.
--
-- Warning: the timeout callback may fire even after
-- 'unregisterTimeout' completes.
unregisterTimeout :: Manager -> TimeoutKey -> IO ()
unregisterTimeout mgr (TK key) =
    postMgr mgr $ modifyTQ $ Q.delete key

------------------------------------------------------------------------
-- The Mgr state monad

newtype Mgr a = Mgr { runMgr :: TimeoutQueue -> IO (a, TimeoutQueue) }

instance Monad Mgr where
    return a = Mgr $ \s -> return (a, s)
    m >>= k = Mgr $ \s -> do
        (a, s') <- runMgr m s
        runMgr (k a) s'

liftIO :: IO a -> Mgr a
liftIO io = Mgr $ \s -> do
    a <- io
    return (a, s)

getsTQ :: (TimeoutQueue -> a) -> Mgr a
getsTQ f = Mgr $ \s -> return (f s, s)

modifyTQ :: (TimeoutQueue -> TimeoutQueue) -> Mgr ()
modifyTQ f = Mgr $ \s -> do
    let !s' = f s
    return ((), s')

stateTQ :: (TimeoutQueue -> (a, TimeoutQueue)) -> Mgr a
stateTQ f = Mgr $ \s -> do
    let (a, !s') = f s
    return (a, s')

------------------------------------------------------------------------
-- I/O manager loop

-- | Call all expired timeouts, and return how much time until the next expiration.
runExpiredTimeouts :: Manager -> Mgr (Maybe Seconds)
runExpiredTimeouts Manager{..} = do
    empty <- getsTQ Q.null
    if empty then
        return Nothing
    else do
        now <- liftIO $ getTime mgrClock
        stateTQ (Q.atMost now) >>= mapM_ (liftIO . Q.value)
        next <- getsTQ $ fmap Q.prio . Q.findMin
        case next of
            Nothing ->
                return Nothing
            Just t -> do
                -- This value will always be positive since the call
                -- to 'atMost' above removed any timeouts <= 'now'
                let !t' = t - now
                return $ Just t'

-- | Return the delay argument to pass to GetQueuedCompletionStatus.
fromTimeout :: Maybe Seconds -> Word32
fromTimeout Nothing                 = 120000
fromTimeout (Just sec) | sec > 120  = 120000
                       | sec > 0    = ceiling (sec * 1000)
                       | otherwise  = 0

step :: Manager -> Mgr ()
step mgr@Manager{..} = do
    delay <- runExpiredTimeouts mgr
    m <- liftIO $ FFI.getNextCompletion mgrIOCP (fromTimeout delay)
    case m of
        Nothing                      -> return ()
        Just (cb, numBytes, errCode) -> cb errCode numBytes

loop :: Manager -> IO loop
loop mgr = go Q.empty
  where go s = runMgr (step mgr) s >>= go . snd

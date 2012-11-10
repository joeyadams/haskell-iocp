module IOCP.Clock (
    Clock,
    Seconds,
    getTime,
    getClock,

    -- * Specific implementations
    queryPerformanceCounter,
    getTickCount64,
    getTickCount,
) where

import qualified IOCP.FFI as FFI

import Control.Monad    (liftM)
import Data.IORef
import Data.Int     (Int32)
import Data.Word    (Word32, Word64)

-- | Monotonic clock
newtype Clock = Clock (IO Seconds)

type Seconds = Double

-- | Get the current time, in seconds since some fixed time in the past.
getTime :: Clock -> IO Seconds
getTime (Clock io) = io

-- | Figure out what time API to use, and return a 'Clock' for accessing it.
getClock :: IO Clock
getClock = tryInOrder
           [ queryPerformanceCounter
           , getTickCount64
           , fmap Just getTickCount
           ]

tryInOrder :: Monad m => [m (Maybe a)] -> m a
tryInOrder (x:xs) = x >>= maybe (tryInOrder xs) return
tryInOrder []     = undefined

mapJust :: Monad m => m (Maybe a) -> (a -> b) -> m (Maybe b)
mapJust m f = liftM (fmap f) m

queryPerformanceCounter :: IO (Maybe Clock)
queryPerformanceCounter =
    FFI.queryPerformanceFrequency `mapJust` \freq ->
    Clock $ do
        count <- FFI.queryPerformanceCounter
        return $! fromIntegral count / fromIntegral freq

getTickCount64 :: IO (Maybe Clock)
getTickCount64 =
    FFI.loadGetTickCount64 `mapJust` \gtc64 ->
    Clock $ do
        msec <- gtc64
        return $! fromIntegral msec / 1000

getTickCount :: IO Clock
getTickCount = do
    -- Work around GetTickCount's 49.7-day wraparound by maintaining a 64-bit
    -- counter and updating it every time the clock is used.
    -- Only works if getTickCount is called at least once every 24.8 days.
    count64 <- FFI.getTickCount >>= newIORef . fromIntegral :: IO (IORef Word64)
    return $ Clock $ do
        msec <- FFI.getTickCount >>= atomicModifyIORef count64 . step
        return $! fromIntegral msec / 1000
  where
    step :: Word32 -> Word64 -> (Word64, Word64)
    step now before = (now64, now64)
      where
        -- Compute the amount of time that has passed since getTickCount was
        -- called last.  Subtract times modulo 2^32, to handle wraparound
        -- properly.  However, convert the offset from unsigned to signed,
        -- to avoid a bogus result if now is earlier than before
        -- (which should never happen).
        offset = fromIntegral (now - fromIntegral before :: Word32) :: Int32

        -- Add the offset to the previous time.
        now64 = before + fromIntegral offset

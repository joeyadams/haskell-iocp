{-# LANGUAGE RecordWildCards #-}
module IOCP.Manager (
    Manager,
    IOCPHandle,
    new,
    associate,

    Overlapped,
    Completion(..),
) where

import IOCP.Worker (Worker, forkOSUnmasked)
import qualified IOCP.FFI    as FFI
import qualified IOCP.Worker as Worker

import Control.Monad        (forever)
import Data.IORef
import Foreign.Ptr          (Ptr)
import GHC.Windows          (iNFINITE)
import System.IO.Unsafe     (unsafeInterleaveIO)
import System.Win32.Types   (DWORD, ErrCode, HANDLE)

type ManagerCallback = Completion -> IO ()

data Manager = Manager
    { managerCompletionPort :: !(FFI.IOCP ManagerCallback)
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
            Just (cb, completionBytes, completionError) ->
                cb Completion{..}
    return Manager{..}

-- | Nifty trick to allow each 'IOCPHandle' to allocate workers per concurrent
-- task, while allowing all 'IOCPHandles' to share the thread pool as a whole.
newThreadPool :: IO [Worker]
newThreadPool = unsafeInterleaveIO $ do
    w  <- Worker.new
    ws <- newThreadPool
    return (w:ws)

associate :: Manager -> HANDLE -> IO IOCPHandle
associate Manager{..} h = do
    FFI.associateHandleWithIOCP managerCompletionPort h
    pool <- newIORef managerThreadPool
    return IOCPHandle
        { iHandle     = h
        , iThreadPool = pool
        }

data IOCPHandle = IOCPHandle
    { iHandle     :: !HANDLE
    , iThreadPool :: !(IORef [Worker])
    }

-- | Corresponds to @LPOVERLAPPED@
type Overlapped = Ptr ()

data Completion = Completion
    { completionError :: !ErrCode
        -- ^ 0 indicates success
    , completionBytes :: !DWORD
    }
    deriving Show

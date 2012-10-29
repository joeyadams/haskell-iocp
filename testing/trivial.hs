-- Compile with -threaded, or you'll get a pattern match failure.
-- This library assumes a threaded runtime system.

{-# OPTIONS -fno-warn-missing-signatures #-}
import Network.Socket hiding (socket, connect)
import Winsock

import Control.Concurrent   (threadDelay)
import Control.Monad    (forever)
import System.Timeout   (timeout)

googleIP = "74.125.140.138"

main = do
    forever $ timeout 1000000 $ do
        putStrLn "Connecting"
        sock <- socket AF_INET Stream defaultProtocol
        addr <- inet_addr googleIP
        connect sock (SockAddrInet 1234 addr)
        close sock

        -- Avoid making successful connections repeatedly.
        threadDelay 10000000

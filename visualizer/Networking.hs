module Networking
( listenLoop
)
where

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Network
import System.IO (Handle)

type Handler = Handle -> HostName -> PortNumber -> IO ()

listenLoop :: PortID -> Handler -> IO ()
listenLoop port handler = listenOn port >>= acceptLoop handler

acceptLoop :: Handler -> Socket -> IO ()
acceptLoop handler socket = forever $ do
    (handle, hostname, portnumber) <- accept socket
    forkIO $ handler handle hostname portnumber

module Networking
( listenLoop
)
where

import Control.Applicative
import Control.Concurrent
import Network
import System.IO

type Handler = Handle -> HostName -> PortNumber -> IO ()

listenLoop :: PortID -> Handler -> IO ()
listenLoop port handler = do
    socket <- listenOn port
    acceptLoop socket handler

acceptLoop :: Socket -> Handler -> IO ()
acceptLoop socket handler = do
    (handle, hostname, portnumber) <- accept socket
    forkIO $ handler handle hostname portnumber
    acceptLoop socket handler

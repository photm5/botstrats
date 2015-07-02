{-# LANGUAGE OverloadedStrings #-}

module Main where

import Commands (handle)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, withMVar)
import Control.Monad (forever, forM_)
import qualified Data.ByteString.Char8 as B
import Data.Monoid ((<>))
import Robots
import Messages
import Mutables
import Network (PortID(PortNumber), connectTo)
import System.IO (Handle)

main = do
    mutables <- initialMutables =<< connectTo "localhost" (PortNumber 2003)
    buffer <- newMVar B.empty
    forever $ do
        maybeMessage <- withMVar (server mutables) $ recv buffer
        case maybeMessage of
            Nothing -> return () -- ignore
            Just message -> do
                waiters <- readMVar (waiters mutables)
                forM_ (filter (isAwaited message) waiters) $ \(Waiter _ f) -> do
                    f message
                handle mutables message

    where isAwaited message waiter = mId message == awaitedId waiter

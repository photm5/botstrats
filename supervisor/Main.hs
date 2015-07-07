{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Commands (handle)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, withMVar)
import Control.Exception (SomeException, catch)
import Control.Monad (forever, forM_, when)
import qualified Data.ByteString.Char8 as B
import Data.Monoid ((<>))
import Robots
import Messages
import Mutables
import Network (PortID(PortNumber), connectTo)
import System.IO (Handle)

main = do
    mutables <- initialMutables =<< connectTo "localhost" (PortNumber 2001)
    (forMessages' (withMVar $ server mutables) $ \m -> case m of
        Nothing -> return ()
        Just message -> do
            waiters <- readMVar (waiters mutables)
            forM_ waiters $ \(Waiter i f) -> do
                when (mId message == i) $ f message
            handle mutables message)
        `catch` \(e :: SomeException) ->
            readMVar (robots mutables) >>= mapM_ killRobot

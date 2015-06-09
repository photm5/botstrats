module Main where

import Data.List (intersperse)
import Data.UUID
import Data.UUID.V4 (nextRandom)
import Control.Concurrent.MVar (newMVar, modifyMVar_)
import Control.Monad (forever)
import Network (PortID(PortNumber))
import System.IO (hPutStrLn, hGetContents, Handle())

import Commands
import Game
import Messages
import Networking

main = do
    var <- initialMVar
    listenLoop (PortNumber 2001) $ \handle host port -> do
        clientId <- nextRandom
        (newMessage "welcome" [show clientId]) >>= send handle
        forever $ do
            msg <- recv handle
            case msg of
                Nothing -> (newMessage "invalid" []) >>= send handle
                Just message -> modifyMVar_ var $ respond message handle

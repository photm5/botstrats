{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B
import Data.List (intersperse)
import Control.Concurrent.MVar (newMVar, modifyMVar_)
import Control.Monad (forever)
import Network (PortID(PortNumber))
import System.IO (hPutStrLn, hGetContents, Handle())

import Commands
import Game
import Messages
import Networking
import UUID

main = do
    var <- initialMVar
    listenLoop (PortNumber 2001) $ \handle host port -> do
        clientId <- randomUUID
        newMessage "welcome" [clientId] >>= send handle
        modifyMVar_ var $ initHeadquarters handle
        forMessages (return handle) $ \m -> case m of
            Nothing -> newMessage "invalid" [] >>= send handle
            Just message -> modifyMVar_ var $ respond message handle

initHeadquarters :: Handle -> GameState -> IO GameState
initHeadquarters handle state = do
    pos <- randomPosition ((0,0), (100,100))
    uuid <- randomUUID
    newMessage "spawn" ["headquarters", uuid] >>= send handle
    newMessage "start" [uuid] >>= send handle
    return $ spawnRobot (Robot uuid pos Idle Headquarters) state

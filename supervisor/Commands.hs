{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Commands
( handle
) where

import qualified Data.ByteString.Char8 as B
import Data.Maybe (listToMaybe, isJust)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (SomeException, catch)
import Control.Lens
import Control.Monad.State
import Control.Monad.Reader
import Messages
import Mutables
import Robots
import System.IO (Handle, hFlush, hIsOpen)

type CmdMonad = ReaderT Mutables (StateT [Robot] IO)

handle :: Mutables -> Message -> IO ()
handle mutables msg = modifyMVar_ (robots mutables) $ \robs ->
    snd <$> runStateT (runReaderT (cmd msg) mutables) robs

cmd :: Message -> CmdMonad ()
cmd (Message _ "spawn" (kind:identifier:[])) = do
    robot <- liftIO $ createRobot kind identifier
    modify (robot:)
cmd (Message _ "start" (identifier:[])) = do
    maybeRobot <- gets $ findOffRobot identifier
    case maybeRobot of
        Nothing -> return ()
        Just offRobot -> do
            maybeRobot <- liftIO $ startRobot offRobot
            mutables <- ask
            case maybeRobot of
                Nothing -> do
                    message <- liftIO $ newMessage "robot_stopped" [identifier]
                    liftIO . withMVar (server mutables) $ flip send message
                Just r -> do
                    byIdentifier identifier .= r
                    void . liftIO . forkIO $ handleRobot mutables r
cmd _ = return ()

handleRobot :: Mutables -> Robot -> IO ()
handleRobot (Mutables server robots waiters) (Running ident stdIn stdOut proc) =
    (forever $ do
        line <- B.hGetLine stdOut
        case B.words line of
            [] -> return () -- ignore
            (command:args) -> do
                message <- newMessage "action" $ [command, ident] ++ args
                withMVar server $ flip send message
                registerWaiter waiters . Waiter (mId message) $ \msg -> do
                    case mParts msg of
                        [] -> return () -- ignore
                        (_:_:results) -> do
                            bots <- readMVar robots
                            when (isJust $ findRunningRobot ident bots) $ do
                                B.hPutStrLn stdIn $ B.unwords results
                                hFlush stdIn)
    `catch` \ (e :: SomeException) -> do
        message <- newMessage "robot_stopped" [ident]
        withMVar server $ flip send message

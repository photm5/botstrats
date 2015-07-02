{-# LANGUAGE OverloadedStrings #-}

module Commands
( handle
) where

import qualified Data.ByteString.Char8 as B
import Data.Maybe (listToMaybe)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad.State
import Control.Monad.Reader
import Messages
import Mutables
import Robots
import System.IO (Handle, hFlush)

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
            robot <- liftIO $ startRobot offRobot
            mutables <- ask
            void . liftIO . forkIO $ handleRobot mutables robot
cmd _ = return ()

handleRobot :: Mutables -> Robot -> IO ()
handleRobot (Mutables server robots waiters) (Running ident stdIn stdOut proc) =
    forever $ do
        line <- B.hGetLine stdOut
        case B.words line of
            [] -> return () -- ignore
            (command:args) -> do
                message <- newMessage "action" $ [command, ident] ++ args
                withMVar server $ flip send message
                registerWaiter waiters . Waiter (mId message) $ \msg -> do
                    case parts msg of
                        [] -> return () -- ignore
                        (_:_:results) -> do
                            B.hPutStrLn stdIn $ B.unwords results
                            hFlush stdIn

findOffRobot :: B.ByteString -> [Robot] -> Maybe Robot
findOffRobot identifier = listToMaybe . filter go
    where go (Off ident) = ident == identifier
          go _ = False

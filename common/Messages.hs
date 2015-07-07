{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Messages where

import Control.Concurrent.MVar (MVar, newMVar)
import Control.Monad (forever)
import qualified Data.ByteString.Char8 as B
import Data.List (intersperse)
import Data.Monoid ((<>))
import Data.UUID (UUID, fromString)
import Data.UUID.V4 (nextRandom)
import GetLineNonBlocking
import System.IO (Handle, hPutStrLn, hGetLine)

data Message = Message { mId :: B.ByteString
               , command :: B.ByteString
               , mParts :: [B.ByteString]
               }

instance Show Message where
    show message = B.unpack . B.concat . intersperse " " $
        (mId message) : command message : mParts message

parseMessage :: B.ByteString -> Maybe Message
parseMessage = parseMessage' . B.words

parseMessage' :: [B.ByteString] -> Maybe Message
parseMessage' [] = Nothing
parseMessage' (_:[]) = Nothing
parseMessage' (x:y:zs) = Just $ Message x y zs

sendInvalid :: Message -> Handle -> IO ()
sendInvalid message handle = send handle $ Message (mId message) "invalid" []

send :: Handle -> Message -> IO ()
send handle message = sequence_ $
    ($ show message) <$> [hPutStrLn handle, putStrLn . ("-> " ++)]

newMessage :: B.ByteString -> [B.ByteString] -> IO Message
newMessage command parts = do
    uuid <- nextRandom
    return $ Message (B.pack $ show uuid) command parts

recv :: Handle -> IO (Maybe Message)
recv handle = do
    line <- B.hGetLine handle
    B.putStrLn $ "<- " <> line
    return $ parseMessage line

recv' :: MVar B.ByteString -> Handle -> IO (Maybe (Maybe Message))
recv' buffer handle = do
    maybeLine <- hGetLineNonBlocking buffer handle
    case maybeLine of
        Nothing -> return Nothing
        Just line -> do
            B.putStrLn $ "<- " <> line
            return . Just $ parseMessage line

forMessages :: IO Handle -> (Maybe Message -> IO ()) -> IO ()
forMessages h f = forever $ h >>= recv >>= f

forMessages' :: (forall a. (Handle -> IO a) -> IO a) -> (Maybe Message -> IO ()) -> IO ()
forMessages' handle fun = do
    buffer <- newMVar B.empty
    forever $ do
        m <- handle $ recv' buffer
        case m of
            Nothing -> return ()
            Just x -> fun x

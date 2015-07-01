{-# LANGUAGE OverloadedStrings #-}

module Messages where

import qualified Data.ByteString.Char8 as B
import Data.List (intersperse)
import Data.Monoid ((<>))
import Data.UUID (UUID, fromString)
import Data.UUID.V4 (nextRandom)
import System.IO (Handle, hPutStrLn, hGetLine)

data Message = Message { mId :: B.ByteString
               , command :: B.ByteString
               , parts :: [B.ByteString]
               }

instance Show Message where
    show message = B.unpack . B.concat . intersperse " " $
        (mId message) : command message : parts message 

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

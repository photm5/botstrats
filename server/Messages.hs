module Messages where

import Data.List (intersperse)
import Data.UUID (UUID, fromString)
import Data.UUID.V4 (nextRandom)
import System.IO (Handle, hPutStrLn, hGetLine)

data Message = Message { mId :: String
               , command :: String
               , parts :: [String]
               }

instance Show Message where
    show message = concat . intersperse " " $ (show . mId $ message) : command message : parts message 

parseMessage :: String -> Maybe Message
parseMessage = parseMessage' . words

parseMessage' :: [String] -> Maybe Message
parseMessage' [] = Nothing
parseMessage' (_:[]) = Nothing
parseMessage' (x:y:zs) = Just $ Message x y zs

sendInvalid :: Message -> Handle -> IO ()
sendInvalid message handle = send handle $ Message (mId message) "invalid" []

send :: Handle -> Message -> IO ()
send handle message = sequence_ $
    ($ show message) <$> [hPutStrLn handle, putStrLn . ("-> " ++)]

newMessage :: String -> [String] -> IO Message
newMessage command parts = do
    uuid <- nextRandom
    return $ Message (show uuid) command parts

recv :: Handle -> IO (Maybe Message)
recv handle = do
    line <- hGetLine handle
    putStrLn $ "<- " ++ line
    return $ parseMessage line

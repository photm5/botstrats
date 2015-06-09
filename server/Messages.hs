module Messages where

import Data.List (intersperse)
import Data.UUID (UUID, fromString)
import Data.UUID.V4 (nextRandom)
import System.IO (Handle, hPutStrLn, hGetLine)
import Text.Parsec
import Text.Parsec.String

data Message = Message { mId :: UUID
               , command :: String
               , parts :: [String]
               }

instance Show Message where
    show message = concat . intersperse " " $ (show . mId $ message) : command message : parts message 

parseMessage :: String -> Maybe Message
parseMessage s = either (const Nothing) Just $ parse messageParser "" s

messageParser :: Parser Message
messageParser = do
    id <- many (noneOf " ")
    uuid <- maybe (fail "") return $ fromString id
    char ' '
    command <- many (noneOf " ")
    optional $ char ' '
    parts <- sepBy (many $ noneOf " ") $ char ' '
    return $ Message uuid command parts

partsParser :: Parser [String]
partsParser = return []

sendInvalid :: Message -> Handle -> IO ()
sendInvalid message handle = send handle $ Message (mId message) "invalid" []

send :: Handle -> Message -> IO ()
send handle message = sequence_ $
    ($ show message) <$> [hPutStrLn handle, putStrLn . ("-> " ++)]

newMessage :: String -> [String] -> IO Message
newMessage command parts = do
    uuid <- nextRandom
    return $ Message uuid command parts

recv :: Handle -> IO (Maybe Message)
recv handle = do
    line <- hGetLine handle
    putStrLn $ "<- " ++ line
    return $ parseMessage line

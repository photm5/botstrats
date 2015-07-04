{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as BS
import Data.Monoid ((<>))
import Network
import System.IO

data Robot = Robot { identifier :: String
                   , kind :: String
                   , x :: String
                   , y :: String
                   , status :: String
                   }

instance FromJSON Robot where
    parseJSON (Object o) = Robot <$>
        o .: "uuid" <*>
        o .: "type" <*>
        o .: "x" <*>
        o .: "y" <*>
        o .: "status"
    parseJSON _ = mzero

main = do
    listenSock <- listenOn (PortNumber 2003)
    (supervisor, _, _ ) <- accept listenSock
    server <- connectTo "localhost" (PortNumber 2001)
    visualizer <- connectTo "localhost" (PortNumber 2000)
    forkIO . forLines supervisor $ B.hPutStrLn server
    forLines server $ \line -> do
        let msg = B.words line
        B.hPutStrLn supervisor line
        if tryIndex msg 1 == (Just "result") &&
           tryIndex msg 2 == (Just "scan") &&
           tryIndex msg 4 /= (Just "success") &&
           tryIndex msg 4 /= (Just "end_of_stream")
        then do
            let maybeRobot = decode . B.unwords $ drop 4 msg
            case maybeRobot of
                Nothing -> B.putStrLn $ "json parsing failed: " <> line
                Just r -> do
                    putStrLn "sending line"
                    hPutStrLn visualizer $ unwords
                        [identifier r, kind r <> ":" <> status r, "1", "1", x r, y r]
        else B.putStrLn line

forLines :: Handle -> (B.ByteString -> IO ()) -> IO ()
forLines h f = B.hGetContents h >>= mapM_ f . B.lines

tryIndex :: [a] -> Int -> Maybe a
tryIndex xs i
    | i < length xs = Just $ xs !! i
    | otherwise      = Nothing

hGetLineNonBlocking :: Handle -> IO (Maybe B.ByteString)
hGetLineNonBlocking handle = do
    readString <- hGetLineNonBlocking' handle B.empty
    if readString /= B.empty && B.last readString == '\n' then
        return $ Just readString
    else return Nothing

hGetLineNonBlocking' :: Handle -> B.ByteString -> IO B.ByteString
hGetLineNonBlocking' handle buffer = do
    chunk <- B.hGetNonBlocking handle 1
    if chunk == B.empty then
        return buffer
    else if chunk == "\n" then
        return buffer
    else return $ buffer <> chunk

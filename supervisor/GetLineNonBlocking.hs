{-# LANGUAGE OverloadedStrings #-}

module GetLineNonBlocking where

import Control.Applicative ((<$>))
import Control.Arrow (second)
import Control.Concurrent.MVar (MVar, modifyMVar_, readMVar)
import qualified Data.ByteString.Char8 as B
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import System.IO (Handle)

hGetLineNonBlocking :: MVar B.ByteString -> Handle -> IO (Maybe B.ByteString)
hGetLineNonBlocking buffer handle = do
    buf <- readMVar buffer
    (isDone, read) <- hGetLineNonBlocking' handle
    if isDone then do
        modifyMVar_ buffer . const $ return B.empty
        return . Just $ buf <> read
    else do
        modifyMVar_ buffer $ return . (<> read)
        return Nothing

hGetLineNonBlocking' :: Handle -> IO (Bool, B.ByteString)
hGetLineNonBlocking' handle = do
    character <- (listToMaybe . B.unpack) <$> B.hGetNonBlocking handle 1
    case character of
        Nothing -> return (False, B.empty)
        Just '\n' -> return (True, B.empty)
        Just x -> second (B.pack [x] <>) <$> hGetLineNonBlocking' handle

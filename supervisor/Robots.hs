{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Robots
( Robot(..)
, rIdentifier
, rStdin
, rStdout
, rProcess
, startRobot
, createRobot
, isRunning
, findRunningRobot
, findOffRobot
, changeRobot
, byIdentifier
) where

import Control.Applicative ((<$>), (<*>))
import Control.Lens
import Control.Monad (forM_, unless, when, mfilter)
import qualified Data.ByteString.Char8 as B
import Control.Exception (SomeException, catch)
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import System.Directory
import System.FilePath.Posix ((</>))
import System.Process
import System.IO (Handle)

data Robot = Running { _rIdentifier :: B.ByteString
                     , _rStdin :: Handle
                     , _rStdout :: Handle
                     , _rProcess :: ProcessHandle
                     }
           | Off { _rIdentifier :: B.ByteString }

makeLenses ''Robot

startRobot :: Robot -> IO (Maybe Robot)
startRobot (Off identifier) = (packInRobot <$> createProcess process)
        `catch` \(e :: SomeException) -> return Nothing
    where packInRobot (Just stdin, Just stdout, Nothing, procHandle) =
              Just $ Running identifier stdin stdout procHandle
          drivePath = genDrivePath identifier
          process = CreateProcess
              -- Use the file `init` in the robot’s drive
              (RawCommand (drivePath <> "/init") [])
              (Just drivePath) -- Use the robot’s drive as working directory
              Nothing          -- Pass on the environment
              CreatePipe       -- Create a pipe for stdin
              CreatePipe       -- Create a pipe for stdout
              Inherit          -- Inherit stderr
              False            -- Don’t close any file descriptors
              False            -- Don’t create a new process group
              True             -- Delegate Control-C handling

createRobot :: B.ByteString -> B.ByteString -> IO Robot
createRobot kind identifier = do
    -- The `True` means parents will be created as well
    createDirectoryIfMissing True (genDrivePath identifier)
    initDir <- (<> "/supervisor/init/" <> B.unpack kind) <$> getCurrentDirectory
    exists <- doesDirectoryExist initDir
    when exists $ copyRecursive initDir (genDrivePath identifier)
    return $ Off identifier

copyRecursive :: FilePath -> FilePath -> IO ()
copyRecursive from to = do
    file <- doesFileExist from
    dir <- doesDirectoryExist from
    if file then do
        copyFile from to
    else if dir then do
        createDirectoryIfMissing True to
        entries <- getDirectoryContents from
        forM_ entries $ \entry -> unless (entry `elem` [".", ".."]) $
            copyRecursive (from </> entry) (to </> entry)
    else return ()

genDrivePath :: B.ByteString -> FilePath
genDrivePath ident = B.unpack $ "/tmp/botstrats/supervisor/drives/" <> ident

hasIdent :: B.ByteString -> Robot -> Bool
hasIdent ident robot = robot^.rIdentifier == ident

isRunning :: Robot -> Bool
isRunning (Off _) = False
isRunning (Running _ _ _ _) = True

findOffRobot :: B.ByteString -> [Robot] -> Maybe Robot
findOffRobot i rs = rs^?byIdentifier i.filtered (not . isRunning)

findRunningRobot :: B.ByteString -> [Robot] -> Maybe Robot
findRunningRobot i rs = rs^?byIdentifier i.filtered isRunning

changeRobot :: B.ByteString -> (Robot -> Robot) -> [Robot] -> [Robot]
changeRobot i f = byIdentifier i %~ f

byIdentifier i = traversed.filtered (hasIdent i)

{-# LANGUAGE OverloadedStrings #-}

module Robots
( Robot(..)
, startRobot
, createRobot
) where

import qualified Data.ByteString.Char8 as B
import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM_, unless, when)
import Data.Monoid ((<>))
import System.Directory
import System.FilePath.Posix ((</>))
import System.Process
import System.IO (Handle)

data Robot = Running { rIdentifier :: B.ByteString
                     , rStdin :: Handle
                     , rStdout :: Handle
                     , rProcess :: ProcessHandle
                     }
           | Off B.ByteString

startRobot :: Robot -> IO Robot
startRobot (Off identifier) = packInRobot <$> createProcess process
    where packInRobot (Just stdin, Just stdout, Nothing, procHandle) =
              Running identifier stdin stdout procHandle
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
              False            -- Don’t delegate Control-C handling

createRobot :: B.ByteString -> B.ByteString -> IO Robot
createRobot kind identifier = do
    -- The `True` means parents will be created as well
    createDirectoryIfMissing True (genDrivePath identifier)
    initDir <- (<> "/init/" <> B.unpack kind) <$> getCurrentDirectory
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
